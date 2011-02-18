package Devel::DumpTrace;

use 5.008;
use PadWalker;
use Scalar::Util;
use Text::Shorten;
use Devel::DumpTrace::CachedDisplayedArray;
use Devel::DumpTrace::CachedDisplayedHash;
use Carp;
use strict;
use warnings;
# $| = 1;

use constant DISPLAY_NONE  => 0;  # trace off
use constant DISPLAY_TERSE => 1;  # concise - 1 trace line per stmnt
use constant DISPLAY_GABBY => 4;  # verbose - 2-5 trace lines per stmt
use constant ABBREV_STRONG => 0;  # strong abbreviation of long scalars,
use constant ABBREV_MILD   => 1;  # mild abbreviation      arrays, hashes
use constant ABBREV_NONE   => 2;  # no abbreviation

# include subroutine name in trace output?
use constant OUTPUT_SUB => 1;

# for interpreting list output of  caller
use constant CALLER_PKG => 0;     # package name
use constant CALLER_SUB => 3;     # current subroutine name

our $VERSION = '0.10';
our $ARRAY_ELEM_SEPARATOR = ',';
our $HASH_ENTRY_SEPARATOR = ';';
our $HASH_PAIR_SEPARATOR = '=>';
our $XEVAL_SEPARATOR = ':';
our $SEPARATOR = "-------------------------------------------\n";

my $pid = $$;
our $DUMPTRACE_FH = *STDERR;
our $DB_ARGS_DEPTH = 3;
our %EXCLUDE_PKG = ();
our %INCLUDE_PKG = ('main' => 1);
our (%DEFERRED, $PAD_MY, $PAD_OUR, $TRACE);
our $_GLOBAL_DESTRUCTION = 0;
our $_INIT = 0;

my (@matches);
my @_INC = @lib::ORIG_INC ? @lib::ORIG_INC : @INC;

# these variables are always qualified into the 'main' package,
# regardless of the current package
my %ALWAYS_MAIN = (ENV => 1, INC => 1, ARGV => 1, ARGVOUT => 1,
		   SIG => 1, STDIN => 1, STDOUT => 1, STDERR => 1,);

# used by _qquote below
my %esc = ("\a" => '\a', "\b" => '\b', "\t" => '\t', "\n" => '\n',
	   "\f" => '\f', "\r" => '\r', "\e" => '\e',);

# use PPI by default, if available
$Devel::DumpTrace::NO_PPI
  || $ENV{DUMPTRACE_NOPPI}
  || eval 'use Devel::DumpTrace::PPI;1';          ## no critic (StringyEval)

{
  *Devel::Trace::TRACE = \$TRACE;
  tie $TRACE, 'Devel::DumpTrace::VerboseLevel';
  if (defined $ENV{DUMPTRACE_LEVEL}) {
    $TRACE = $ENV{DUMPTRACE_LEVEL};
  } else {
    $TRACE = 'default';
  }

  *DB::DB = \&DB__DB unless defined &DB::DB;

  if (defined $ENV{DUMPTRACE_FH}) {
    if (uc $ENV{DUMPTRACE_FH} eq 'STDOUT') {
      $DUMPTRACE_FH = *STDOUT;
    } else {
      ## no critic (BriefOpen)
      unless (open $DUMPTRACE_FH, '>', $ENV{DUMPTRACE_FH}) {
	die "Can't use $ENV{DUMPTRACE_FH} as trace output file: $!\n",
	  "Devel::DumpTrace module is quitting.\n";
      }
    }
  }

  _ignore_packages(qr/^Devel::DumpTrace/, qr/^Text::Shorten$/);
  _recognize_packages('^main$');
}

sub import {
  my $class = shift;
  if (@_ > 0) {
    $TRACE = join ',', @_;
  }
  return;
}

{
  # routines for initializing %EXCLUDE_PKG, %INCLUDE_PKG

  my @installed_pkgs = ();
  my %installed_pkgs = ();
  my %_seen = ();

  sub _ignore_packages {
    my (@patterns) = @_;
    foreach my $pattern (@patterns) {
      foreach my $pkg (grep { $_ =~ $pattern } _find_installed_pkgs()) {
	$EXCLUDE_PKG{$pkg}++;
	delete $INCLUDE_PKG{$pkg};
      }
    }
    return;
  }

  sub _recognize_packages {
    my (@patterns) = @_;
    foreach my $pattern (@patterns) {
      foreach my $pkg (grep { $_ =~ $pattern } _find_installed_pkgs()) {
	$INCLUDE_PKG{$pkg}++;
	delete $EXCLUDE_PKG{$pkg};
      }
    }
    return;
  }

  # recursively walk the symbol table and add any namespaces found
  # to %installed_pkgs
  sub _examine_symbol_table_for_installed_pkgs {
    my ($nspace, $ref) = shift;
    return if $_seen{$ref=eval'\%::'.$nspace};
    $_seen{$ref}++;
    $installed_pkgs{$nspace}++;
    for (keys %$ref) {
      _examine_symbol_table_for_installed_pkgs($nspace.$_) if /::$/;
    }
    return;
  }

  sub _find_installed_pkgs {
    if (0 == @installed_pkgs) {
      _examine_symbol_table_for_installed_pkgs('');
      @installed_pkgs = sort keys %installed_pkgs;
      s/::$// foreach @installed_pkgs;
    }
    return @installed_pkgs;
  }
}

sub DB__DB {
  return unless $Devel::DumpTrace::TRACE;

  my ($p, $f, $l) = caller();
  my (undef, undef, undef, $sub) = caller(1);
  $sub ||= '__top__';
  $sub =~ s/::$/::__top__/;

  return if _exclude_pkg($f,$p,$l);
  return if _display_style() == DISPLAY_NONE;
  handle_deferred_output($sub, $f);

  my $code = get_source($f,$l);

  save_pads(1);
  save_previous_regex_matches();
  evaluate_and_display_line($code, $p, $f, $l, $sub);
  return;
}

sub get_source {
  my ($file, $line) = @_;
  no strict 'refs';                    ## no critic (NoStrict)
  my $source = \@{"::_<$file"};
  return $source->[$line];
}

sub _exclude_pkg {
  my($file,$pkg,$line) = @_;

  return 0 if $INCLUDE_PKG{$pkg} || $INCLUDE_PKG{$file};
  return 1 if $EXCLUDE_PKG{$pkg} || $EXCLUDE_PKG{$file};
  return 0 if _package_style() > DISPLAY_NONE;

  # exclude files from @_INC when _package_style() is 0
  foreach my $inc (@_INC) {
    if (index($inc,"/") >= 0 && index($file,$inc) == 0) {
      return $EXCLUDE_PKG{$file} = $EXCLUDE_PKG{$pkg} = 1;
    }
  }
  $INCLUDE_PKG{$file} = 1;
  return 0;
}

# map $TRACE variable to a display style
sub _display_style {
  return (DISPLAY_TERSE,
	  DISPLAY_TERSE,
	  DISPLAY_TERSE,
	  DISPLAY_TERSE,
	  DISPLAY_GABBY,
	  DISPLAY_GABBY,
	  DISPLAY_GABBY,
	  DISPLAY_GABBY,
	  DISPLAY_GABBY,
	  DISPLAY_GABBY)[$TRACE % 10];
}

# map $TRACE variable to an abbreviation style
sub _abbrev_style {
  return (ABBREV_STRONG,
	  ABBREV_STRONG,
	  ABBREV_MILD,
	  ABBREV_NONE,
	  ABBREV_MILD,
	  ABBREV_NONE,
	  ABBREV_NONE,
	  ABBREV_NONE,
	  ABBREV_NONE,
	  ABBREV_NONE,)[$TRACE % 10]
}

sub _package_style {
  return $TRACE >= 100;
}

sub save_pads {
  my $n = shift || 0;
  my $target_depth = current_depth() - $n - 1;

  if ($target_depth < 0) {
    Carp::cluck "save_pads: request for negative frame ",
	current_depth(), " $target_depth $n at ";
    return;
  }

  $PAD_MY = PadWalker::peek_my($n + 1);
  $PAD_OUR = PadWalker::peek_our($n + 1);

  # add extra data to the pads so that they can be refreshed
  # at an arbitrary point in the future
  $PAD_MY->{__DEPTH__} = $PAD_OUR->{__DEPTH__} = current_depth() - $n - 1;

  return;
}

sub current_depth {
  my $n = 0;
  $n++ while caller($n);
  return $n-1;
}

sub refresh_pads {
  return if $_GLOBAL_DESTRUCTION;
  my $current = current_depth();
  my $target = $PAD_MY->{__DEPTH__};
  save_pads($current - $target);
  return;
}

sub evaluate_and_display_line {
  my ($code, $p, $f, $l, $sub) = @_;
  my $style = _display_style();

  if ($style > DISPLAY_TERSE) {
    separate();
    print {$DUMPTRACE_FH} ">>    ", current_position_string($f,$l,$sub), "\n";
    print {$DUMPTRACE_FH} ">>> \t\t $code";  # [orig]
  }

  # look for assignment operator.
  if ($code    =~ m{[-+*/&|^.%]?=[^=>]} 
      || $code =~ m{[\b*&|/<>]{2}=\b}   ) {

    my ($expr1, $op, $expr2) = ($`, $&, $');
    if ($style < DISPLAY_GABBY) {
      $expr2 = perform_extended_variable_substitutions($op . $expr2, $p);
    } else {
      $expr2 = perform_variable_substitutions($op . $expr2, $p);
    }

    $DEFERRED{"$sub : $f"}
      = { PACKAGE => $p,
	  MY_PAD  => $PAD_MY,
	  OUR_PAD => $PAD_OUR,
	  SUB     => $sub,
	  FILE    => $f,
	  LINE    => $l,
          DISPLAY_FILE_AND_LINE => $style <= DISPLAY_TERSE,
	  EXPRESSION => [ $expr1, $expr2 ] 
	};

    if ("$expr1$expr2" ne $code) {
      if ($style >= DISPLAY_GABBY) {
	print {$DUMPTRACE_FH} ">>>> \t\t $expr1$expr2";  # [pre eval]
      }
    }
    return;
  }

  my $xcode;

  # if this is a simple lexical declaration and NOT an assignment,
  # then don't perform variable substitution:
  #          my $k;
  #          my ($a,$b,@c);
  #          our $ZZZ;

  if ($code    =~ /^ \s* (my|our) \s*
                    [\$@%*\(] /x           # lexical declaration
      && $code =~ / (?<!;) .* ;
                    \s* (\# .* )? $/x   # single statement, single line
      && $code !~ /=/) {                # NOT an assignment

    $xcode = $code;

  } else {

    $xcode = perform_variable_substitutions($code, $p);

  }

  if ($style >= DISPLAY_GABBY) {
    if ($xcode ne $code) {
      print {$DUMPTRACE_FH} ">>>> \t\t $xcode";     # [pre eval]
    }
  } elsif ($style == DISPLAY_TERSE) {
    print {$DUMPTRACE_FH} ">>>>  ", 
      current_position_string($f,$l,$sub),"\t\t $xcode";
  }
  return;
}

sub separate {
  if ($Devel::DumpTrace::SEPARATOR_USED++) {
    print {$DUMPTRACE_FH} $SEPARATOR;
  }
  return;
}

sub dump_scalar {
  my $scalar = $_[0];
  # was  my $scalar = shift   and was   my ($scalar) = @_;
  # but they caused "Modification of a read-only value attempted"
  # error with Perl 5.8.8

  return 'undef' if !defined $scalar;
  if (Scalar::Util::looks_like_number($scalar)) {
    $scalar =~ s/^\s+//;
    $scalar =~ s/\s+$//;
    return _abbreviate_scalar($scalar);
  }
  if (ref $scalar) {
    if (Scalar::Util::reftype($scalar) eq 'ARRAY') {
      return '[' . array_repr($scalar) . ']';
    }
    if (Scalar::Util::reftype($scalar) eq 'HASH') {
      return '{' . hash_repr($scalar) . '}';
    }
    return $scalar;
  }
  if (ref \$scalar eq 'GLOB') {
    return $scalar;
  }
  my $qq = _qquote($scalar);
  if ($qq ne $scalar) {
    return _abbreviate_scalar(qq("$qq"));
  }
  return _abbreviate_scalar(qq('$scalar'));
}

sub _abbreviate_scalar {
  my ($value) = @_;
  if (_abbrev_style() >= ABBREV_NONE) {
    return $value;
  }
  if (_abbrev_style() >= ABBREV_MILD) {
    # mild abbreviation: no token longer than 80 chars
    return Text::Shorten::shorten_scalar($value, 80);
  } else {
    # strong abbreviation: no token longer than 20 chars
    return Text::Shorten::shorten_scalar($value, 20);
  }
}

# shamelessly lifted from Data::Dumper::qquote
#
# converts a string of arbitrary characters to an ASCII string that
# produces the original string under double quote interpolation
sub _qquote {
  local($_) = shift;
  s/([\\\"\@\$])/\\$1/g;
  my $bytes; { use bytes; $bytes = length }
  ($bytes > length) && s/([^\x00-\x7f])/'\x{'.sprintf("%x",ord($1)).'}'/ge;
  /[^ !"\#\$%&'()*+,\-.\/0-9:;<=>?\@A-Z[\\\]^_`a-z{|}~]/ || return $_;

  my $high = shift || '';
  s/([\a\b\t\n\f\r\e])/$esc{$1}/g;

  if (ord('^')==94)  {
    # no need for 3 digits in escape for these
    s/([\0-\037])(?!\d)/'\\'.sprintf('%o',ord($1))/eg;
    s/([\0-\037\177])/'\\'.sprintf('%03o',ord($1))/eg;
    # all but last branch below not supported --BEHAVIOR SUBJECT TO CHANGE--
    if ($high eq 'iso8859') {
      s/([\200-\240])/'\\'.sprintf('%o',ord($1))/eg;
    } elsif ($high eq 'utf8') {
#     use utf8;
#     $str =~ s/([^\040-\176])/sprintf "\\x{%04x}", ord($1)/ge;
    } elsif ($high eq '8bit') {
        # leave it as it is
    } else {
      s/([\200-\377])/'\\'.sprintf('%03o',ord($1))/eg;
      s/([^\040-\176])/sprintf "\\x{%04x}", ord($1)/ge;
    }
  } else { # ebcdic
    s{([^ !"\#\$%&'()*+,\-.\/0-9:;<=>?\@A-Z[\\\]^_`a-z{|}~])(?!\d)}
    {my $v = ord($1); '\\'.sprintf(($v <= 037 ? '%o' : '%03o'), $v)}eg;
    s{([^ !"\#\$%&'()*+,\-.\/0-9:;<=>?\@A-Z[\\\]^_`a-z{|}~])}
     {'\\'.sprintf('%03o',ord($1))}eg;
  }
  return $_;
}

sub hash_repr {
  my $hashref = shift;

  return '' if !defined $hashref;
  my $ref = ref $hashref && ref $hashref ne 'HASH'
    ? ref($hashref) . ': ' : '';
  my $maxlen = _abbrev_style() < ABBREV_NONE
    ? _abbrev_style() > ABBREV_STRONG ? 79 : 19 : -1;
  my $cache_key = join ':', 
    $maxlen, $HASH_ENTRY_SEPARATOR, $HASH_PAIR_SEPARATOR;
  my $hash;

  # When the hash table gets large, tie it to 
  # Devel::DumpTrace::CachedDisplayedHash and
  # see if we can avoid some expensive calls
  # to  Text::Shorten::shorten_hash .

  if (tied(%{$hashref})
      && ref(tied %{$hashref}) eq 'Devel::DumpTrace::CachedDisplayedHash') {

    my $result = (tied %{$hashref})->get_cache($cache_key);
    if (defined $result) {
      return $ref . join $HASH_ENTRY_SEPARATOR,
	map { join $HASH_PAIR_SEPARATOR, @{$_} } @{$result};
    }
    $hash = (tied %{$hashref})->{PHASH};
  } elsif (!tied(%{$hashref}) && 100 < scalar keys %{$hashref}) {
    tie %{$hashref}, 'Devel::DumpTrace::CachedDisplayedHash', %{$hashref};
    $hash = (tied %{$hashref})->{PHASH};
  } else {
    $hash = { map { dump_scalar($_) => dump_scalar($hashref->{$_}) }
	      keys %{$hashref} };
  }

  my @r;

  if (_abbrev_style() < ABBREV_NONE) {
    @r = Text::Shorten::shorten_hash(
		$hash, $maxlen,
		$HASH_ENTRY_SEPARATOR,
		$HASH_PAIR_SEPARATOR );
  } else {
    @r = map { [ $_ => $hash->{$_} ] } keys %{$hash};
  }

  if (tied(%{$hashref})
      && ref(tied %{$hashref}) eq 'Devel::DumpTrace::CachedDisplayedHash') {
    (tied %{$hashref})->store_cache($cache_key, \@r);
  }

  return $ref . join $HASH_ENTRY_SEPARATOR,
    map { join $HASH_PAIR_SEPARATOR, @{$_} } @r;
}

sub array_repr {
  my $arrayref = shift;

  return '' if !defined $arrayref;
  my $ref = ref $arrayref && ref $arrayref ne 'ARRAY'
    ? ref($arrayref) . ': ' : '';
  my $maxlen = _abbrev_style() < ABBREV_NONE
    ? _abbrev_style() > ABBREV_STRONG ? 79 : 19 : -1;
  my $cache_key = join ':', $maxlen, $ARRAY_ELEM_SEPARATOR;
  my $array;

  # When the array gets large, tie it to 
  # Devel::DumpTrace::CachedDisplayedArray and
  # see if we can avoid some expensive calls
  # to  Text::Shorten::shorten_array .

  if (tied @{$arrayref}
      && ref(tied @{$arrayref}) eq 'Devel::DumpTrace::CachedDisplayedArray') {

    my $result = (tied @{$arrayref})->get_cache($cache_key);
    if (defined $result) {
      return $ref . join $ARRAY_ELEM_SEPARATOR, @$result;
    }
    $array = (tied @{$arrayref})->{PARRAY};
  } elsif (!tied(@{$arrayref}) && 100 < scalar @{$arrayref}) {
    tie @{$arrayref}, 'Devel::DumpTrace::CachedDisplayedArray', @{$arrayref};
    $array = (tied @{$arrayref})->{PARRAY};
  } else {
    $array = [ map { dump_scalar($_) } @{$arrayref} ];
  }

  my @r;
  if ($maxlen > 0) {
    @r = Text::Shorten::shorten_array( $array, $maxlen, $ARRAY_ELEM_SEPARATOR);
  } else {
    @r = @{$array};
  }
  if (tied(@{$arrayref})
      && ref(tied @{$arrayref}) eq 'Devel::DumpTrace::CachedDisplayedArray') {
    (tied @{$arrayref})->store_cache($cache_key, \@r);
  }
  return $ref . join $ARRAY_ELEM_SEPARATOR, @r;
}

sub handle_ALL_deferred_output {
  foreach my $context (keys %DEFERRED) {
    my ($sub, $file) = split / : /, $context, 2;
    handle_deferred_output($sub, $file);
  }
  return;
}

sub handle_deferred_output {
  my ($sub, $file) = @_;
  my $deferred = delete $DEFERRED{"$sub : $file"};
  if (defined $deferred) {

    my ($expr1, $expr2) = @{$deferred->{EXPRESSION}};
    my $deferred_pkg = $deferred->{PACKAGE};
    $PAD_MY = $deferred->{MY_PAD};
    $PAD_OUR = $deferred->{OUR_PAD};
    refresh_pads();
    $PAD_MY->{__STALE__} = $deferred->{MY_PAD};
    $PAD_OUR->{__STALE__} = $deferred->{OUR_PAD};
    my ($line);
    if ($deferred->{DISPLAY_FILE_AND_LINE}) {
      $file = $deferred->{FILE};
      $line = $deferred->{LINE};
    }
    if (defined($line)) {
      print {$DUMPTRACE_FH} ">>>>> ", 
	current_position_string($file,$line,$deferred->{SUB}), 
	"\t", perform_extended_variable_substitutions($expr1, $deferred_pkg),
	$expr2;
    } else {
      print {$DUMPTRACE_FH} ">>>>>\t\t ",
	perform_variable_substitutions($expr1, $deferred_pkg), $expr2;
    }
  }
  return;
}

sub perform_variable_substitutions {
  my ($xcode, $pkg) = @_;
  $xcode =~ s{  ([\$\@\%])\s*               # sigil
                ([\w:]+)                    # package (optional) and var name
                (\s*->)?                    # optional dereference op
                (\s*[\[\{])?                # optional subscript
             }{ 
                evaluate($1,$2,$3||'',$4||'',$pkg) 
             }gex;

  return $xcode;
}

sub current_position_string {
  my ($file, $line, $sub) = @_;
  if (OUTPUT_SUB) {
    $sub ||= '__top__';
    # $file already probably contains package information.
    # Keeping it in $sub is _usually_ redundant and makes the
    # line too long.
    $sub =~ s/.*:://;
    return "$file:$line:[$sub]:";
  } else {
    return "$file:$line:";
  }
}

sub perform_extended_variable_substitutions {
  my ($xcode, $pkg) = @_;
  $xcode =~ s{  ([\$\@\%])\s*                   # sigil
                ([\w:]+)                        # var name, nay include pkg
                (\s*->)?                        # optional dereference op
                (\s*[\[\{])?                    # optional subscript
             }{ $1 . $2 . $XEVAL_SEPARATOR
               . evaluate($1,$2,$3||'',$4||'',$pkg)
             }gex;
  return $xcode;
}

# McCabe score: 48
sub evaluate {
  my ($sigil, $varname, $deref_op, $index_op, $pkg) = @_;
  my $v;

  no strict 'refs';                    ## no critic (NoStrict)

  $deref_op ||= '';
  $index_op ||= '';
  $index_op =~ s/^\s+//;

  if ($ALWAYS_MAIN{$varname} || $varname =~ /^\d+$/) {
    $pkg = 'main';
  }
  $pkg .= '::';
  if ($varname =~ /::/ || $pkg eq '<magic>::') {
    $pkg = '';
  }

  if ($deref_op) {
    my $sigvar = "\$$varname";
    (my $pkgvar = $sigvar) =~ s/\$/\$$pkg/;

    if (defined $PAD_MY->{$sigvar}) {
      $v = $PAD_MY->{$sigvar};
    } elsif (defined $PAD_OUR->{$sigvar}) {
      $v = $PAD_OUR->{$sigvar};
    } elsif (defined $PAD_MY->{__STALE__}{$sigvar}) {
      $v = $PAD_MY->{__STALE__}{$sigvar};
    } elsif (defined $PAD_OUR->{__STALE__}{$sigvar}) {
      $v = $PAD_OUR->{__STALE__}{$sigvar};
    } else {
      $v = eval "\\$pkgvar";                    ## no critic (StringyEval)
    }
    if ($index_op eq '[') {
      return '[' . array_repr(${$v}) . ']->[';
    }
    if ($index_op eq '{') {
      return '{' . hash_repr(${$v}) . '}->{';
    }

    my $reftype = Scalar::Util::reftype(${$v});
    if (!defined($reftype) || $reftype eq '') {
      return '(' . dump_scalar($v) . ')->';
    } elsif ($reftype eq 'HASH') {
      return '{' . hash_repr(${$v}) . '}->';
    } elsif ($reftype eq 'ARRAY') {
      return '[' . array_repr(${$v}) . ']->';
    } else {
      return '(' . dump_scalar($v) . ')->';
    }
  }

  if ($index_op eq '{') {
    my $sigvar = "\%$varname";
    (my $pkgvar = $sigvar) =~ s/\%/\%$pkg/;
    if (defined($PAD_MY->{$sigvar})) {
      $v = $PAD_MY->{$sigvar};
    } elsif (defined($PAD_OUR->{$sigvar})) {
      $v = $PAD_OUR->{$sigvar};
    } else {
      $v = eval "\\$pkgvar";                    ## no critic (StringyEval)
    }
    return '(' . hash_repr($v) . '){';
  }
  if ($sigil eq '@') {
    my $sigvar = "\@$varname";
    (my $pkgvar = $sigvar) =~ s/\@/\@$pkg/;

    if ($varname eq '_') {
      # calling  caller  (1) with arg, (2) in list context,
      # (3) from DB package will populate @DB::args, which is
      # what we really want.
      my $depth = $DB_ARGS_DEPTH;
      no warnings 'uninitialized';
      while ((caller $depth)[CALLER_SUB] =~ /^\(eval/) {
	$depth++;
      }
      { package DB; () = caller $depth }
      $pkgvar = '@DB::args';
    }

    if (defined($PAD_MY->{$sigvar})) {
      $v = $PAD_MY->{$sigvar};
    } elsif (defined($PAD_OUR->{$sigvar})) {
      $v = $PAD_OUR->{$sigvar};
    } else {
      $v = eval "\\$pkgvar";                    ## no critic (StringyEval)
    }
    if ($index_op eq '[') {
      return '(' . array_repr($v) . ')[';
    }
    return '(' . array_repr($v) . ')';
  }
  if ($sigil eq '%') {
    my $sigvar = "\%$varname";
    (my $pkgvar = $sigvar) =~ s/\%/\%$pkg/;
    if (defined($PAD_MY->{$sigvar})) {
      $v = $PAD_MY->{$sigvar};
    } elsif (defined($PAD_OUR->{$sigvar})) {
      $v = $PAD_OUR->{$sigvar};
    } else {
      $v = eval "\\$pkgvar";                    ## no critic (StringyEval)
    }
    return '(' . hash_repr($v) . ')';
  }
  if ($sigil eq '$') {
    if ($index_op eq '[') {
      my $sigvar = "\@$varname";
      (my $pkgvar = $sigvar) =~ s/\@/\@$pkg/;
      if ($varname eq '_') {
	my $depth = $DB_ARGS_DEPTH;
	{package DB; () = caller $depth};
	$pkgvar = '@DB::args';
      }
      if (defined($PAD_MY->{$sigvar})) {
	$v = $PAD_MY->{$sigvar};
      } elsif (defined($PAD_OUR->{$sigvar})) {
	$v = $PAD_OUR->{$sigvar};
      } else {
	$v = eval "\\$pkgvar";                    ## no critic (StringyEval)
      }
      return '(' . array_repr($v) . ')[';
    } elsif ($varname =~ /^\d+$/) {
      # special regex match var $1,$2,...
      $v = $matches[$varname];
      return dump_scalar($v);
    } else {
      my $sigvar = "\$$varname";
      if ($varname eq '_') {
	$pkg = 'main::';
      }
      (my $pkgvar = $sigvar) =~ s/\$/\$$pkg/;

      if (defined($PAD_MY->{$sigvar})) {
	$v = ${$PAD_MY->{$sigvar}};
      } elsif (defined($PAD_OUR->{$sigvar})) {
	$v = ${$PAD_OUR->{$sigvar}};
      } else {
	$v = eval "$pkgvar";                      ## no critic (StringyEval)
      }
      return dump_scalar($v);
    }
  }

  Carp::confess 'No interpolation done for input: ',
    "<sigil:$sigil ; varname:$varname ; deref:$deref_op ; ",
    "index:$index_op ; pkg:$pkg>\n"
}

sub save_previous_regex_matches {
  @matches = ($0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$10,
	      $11,$12,$13,$14,$15,$16,$17,$18,$19,$20,
	      $21,$22,$23,$24,$25,$26,$27,$28,$29,$30,);

  # XXX - if someone needs more than $30, submit a feature request
  # (http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Devel-DumpTrace,
  # or email to bug-Devel-DumpTrace@rt.cpan.org)
  # and I'll figure something out ...

  return;
}

END {
  $_GLOBAL_DESTRUCTION = 1;
  if ($$ == $pid) {
    no warnings 'redefine';
    handle_ALL_deferred_output();
    separate() if _display_style() > DISPLAY_TERSE;
    untie $TRACE;
    $TRACE = 0;
    *DB::DB = sub { };
    if ($DUMPTRACE_FH ne *STDERR && $DUMPTRACE_FH ne *STDOUT) {
      close $DUMPTRACE_FH;
    }
  }
}

##################################################################

# import Devel::DumpTrace functions to calling namespace. Used in test suite.
sub import_all {
  no strict 'refs';                    ## no critic (NoStrict)
  my $p = caller;
  *{$p . '::save_pads'} = *save_pads;
  *{$p . '::evaluate_and_display_line'} = *evaluate_and_display_line;
  *{$p . '::dump_scalar'} = *dump_scalar;
  *{$p . '::hash_repr'} = *hash_repr;
  *{$p . '::array_repr'} = *array_repr;
  *{$p . '::handle_deferred_output'} = *handle_deferred_output;
  *{$p . '::substitute'} = *perform_variable_substitutions;
  *{$p . '::xsubstitute'} = *perform_extended_variable_substitutions;
  *{$p . '::evaluate'} = *evaluate;
  *{$p . '::save_previous_regex_matches'} = *save_previous_regex_matches;
  return;
}

##################################################################
# Devel::DumpTrace::VerboseLevel: tie class for $Devel::DumpTrace::TRACE.
#
# This class allows us to say, for example,
#
#   $TRACE = 'verbose'
#
# and have the keyword 'verbose' translated into the value "5".
#

package Devel::DumpTrace::VerboseLevel;
use Carp;

sub TIESCALAR {
  my ($pkg) = @_;
  my $scalar;
  return bless \$scalar, $pkg;
}

sub FETCH {
  my $self = shift;
  return ${$self};
}

sub STORE {
  my ($self, $value) = @_;
  my $old = ${$self};
  my ($style, $package) = split /,/, $value;
  $style =~ s/^\s+//;
  $style =~ s/\s+$//;
  $style = {verbose=>5, normal=>3, default=>3,
	    quiet=>1, on=>3, off=>'00'}->{lc $style} || $style;
  if ($style !~ /^\d+$/) {
    carp "Unrecognized debugging level $style\n";
    $style = 3;
  }
  ${$self} = $style;
  if (defined $package) {
    $package =~ s/^\s+//;
    $package =~ s/\s+$//;
    if ($package) {
      ${$self} += 100;
    }
  }
  return $old;
}

1;

__END__

=head1 NAME

Devel::DumpTrace - Evaluate and print out each line before it is executed.

=head1 VERSION

0.10

=head1 SYNOPSIS

    perl -d:DumpTrace program.pl
    perl -d:DumpTrace=verbose program.pl
    perl -d:DumpTrace=quiet program.pl
    perl -d:DumpTrace=<n> program.pl

    perl -d:DumpTrace::PPI program.pl
    perl -d:DumpTrace::noPPI program.pl

=head1 DESCRIPTION

L<Similar to Devel::Trace|Devel::Trace>, this module will cause a message
to be printed to standard error for each line of source code that is
executed. In addition, this module will attempt to identify variable names
in the source code and substitute the values of those variables. In this
way you can say the path of execution through your program as well
as see the value of your variables at each step of the program.

For example, if your program looks like this:

    #!/usr/bin/perl
    # a demonstration of Devel::DumpTrace
    $a = 1;
    $b = 3;
    $c = 2 * $a + 7 * $b;
    @d = ($a, $b, $c + $b);

then the C<DumpTrace> output will look like:

    $ perl -d:DumpTrace demo.pl
    >>>>> demo.pl:3:        $a:1 = 1;
    >>>>> demo.pl:4:        $b:3 = 3;
    >>>>> demo.pl:5:        $c:23 = 2 * $a:1 + 7 * $b:3;
    >>>>> demo.pl:6:        @d:(1,3,26) = ($a:1, $b:3, $c:23 + $b:3);

There are also more I<verbose> modes which will produce even more
detailed output:

    $ perl -d:DumpTrace=verbose demo.pl
    >>  demo.pl:3:
    >>>              $a = 1;
    >>>>>            1 = 1;
    -------------------------------------------
    >>  demo.pl:4:
    >>>              $b = 3;
    >>>>>            3 = 3;
    -------------------------------------------
    >>  demo.pl:5:
    >>>              $c = 2 * $a + 7 * $b;
    >>>>             $c = 2 * 1 + 7 * 3;
    >>>>>            23 = 2 * 1 + 7 * 3;
    -------------------------------------------
    >>  demo.pl:6:
    >>>              @d = ($a, $b, $c + $b);
    >>>>             @d = (1, 3, 23 + 3);
    >>>>>            (1,3,26) = (1, 3, 23 + 3);
    -------------------------------------------

See C<$Devel::DumpTrace::TRACE> under the L</"VARIABLES"> section
for more details about the different levels of verbosity.

This distribution comes with both a basic parser and a
L<PPI-based parser|Devel::DumpTrace::PPI>. If the L<PPI|PPI>
module is installed on your system, then this module will automatically
use the PPI-based parser to analyze the traced code. You can
force this module to use the basic parser by running with the
C<-d:DumpTrace::noPPI> argument or by setting the C<DUMPTRACE_NOPPI>
environment variable:

    # use PPI if available, otherwise use basic parser
    $ perl -d:DumpTrace program.pl

    # fails if PPI is not available
    $ perl -d:DumpTrace::PPI program.pl

    # always uses basic parser
    $ perl -d:DumpTrace::noPPI program.pl
    $ DUMPTRACE_NOPPI=1 perl -d:DumpTrace program.pl

There is also a L<Devel::DumpTrace::PPI|Devel::DumpTrace::PPI> module in this
distribution which relies on L<PPI|PPI> to understand the source code.

See the L</"BUGS AND LIMITATIONS"> section for important, er, limitations
of this module, especially for the basic parser.

=head1 SUBROUTINES/METHODS

None of interest.

=head1 VARIABLES

=head2 C<$Devel::DumpTrace::TRACE>

Controls whether and how much output is produced by this module.
Setting C<$Devel::DumpTrace::TRACE> to zero will disable the module.
Since this module can produce a lot of output and has other overhead
that can considerably slow down your program, you may find it
useful to toggle this variable for critical sections of your code
rather than leave it set for the entire program. For example:

    BEGIN { $Devel::DumpTrace::TRACE = 0 }

    &some_non_critical_code_that_more_or_less_works();

    $Devel::DumpTrace::TRACE = 'normal';
    &the_critial_code_you_want_to_debug();
    $Devel::DumpTrace::TRACE = 0;

    &some_more_non_critical_code();

or to enable tracing in a C<local> block:

    {
        local $Devel::DumpTrace::TRACE = 1;
        &the_critical_code;
    }


In general higher values of C<$Devel::DumpTrace::TRACE> will cause
more output to be produced.
Let's consider this simple program to see how the different
C<$Devel::DumpTrace::TRACE> settings affect the output:

    @a = (1 .. 40);
    $b = $a[4];

=over 4

=item C<$Devel::DumpTrace::TRACE> == 1

is the quietest mode. One line of output for each statement evaluated.
The name of each variable in the source code and its value are included
in the same line of output. Values of long scalars, long arrays, or
long hash tables are heavily abbreviated:

    $ perl -d:DumpTrace=1 littledemo.pl
    >>>>> littledemo.pl:1:[__top__]:  @a:(1,2,3,4,5,6,...,40) = (1 .. 40);
    >>>>> littledemo.pl:2:[__top__]:  $b:5 = $a:(1,2,3,4,5,6,...,40)[4];

=item C<$Devel::DumpTrace::TRACE> == 2

uses a single line of output for each statement evaluated. The name
of each variable in the source code and its source code are included
in the same line of output. Values of long scalar, arrays, and hashes
are less heavily abbreviated.

    $ perl -I. -d:DumpTrace=2 littledemo.pl
    >>>>> littledemo.pl:1:[__top__]:  @a:(1,2,3,4,5,6,7,8,9,10,11,12,13,14, \
        15,16,17,18,19,20,21,22,23,24,25,26,27,...,40) = (1 .. 40);
    >>>>> littledemo.pl:2:[__top__]:  $b:5 = $a:(1,2,3,4,5,6,7,8,9,10,11,12, \
        13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,...,40)[4];

=item C<$Devel::DumpTrace::TRACE> == 3

produces one line of output for each statement evaluated.
The name of each variable in the source code and its
source code are included in the same line of output.
Values of long scalar, arrays, and hashes are B<not>
abbreviated at all. B<This is the default setting for the
module>.

    $ perl -I. -d:DumpTrace=3 littledemo.pl
    >>>>> littledemo.pl:1:[__top__]:  @a:(1,2,3,4,5,6,7,8,9,10,11,12,13,14, \
        15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37, \
        38,39,40) = (1 .. 40);
    >>>>> littledemo.pl:2:[__top__]:  $b:5 = $a:(1,2,3,4,5,6,7,8,9,10,11,12, \
        13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35, \
        36,37,38,39,40)[4];

=item C<$Devel::DumpTrace::TRACE> == 4

produces up to four lines of output for each statement evaluated:

=over 4

=item * the source (file and line number) of the statement being evaluated

=item * the origianl source code for the statement being evaluated

=item * a valuation of the code B<before> the statement has been evaluated
by the Perl interpreter.

=item * a valuation of the code B<after> the statement has been evaluated
by the Perl interpreter

=back

A separator line will also be displayed between statements.
Long scalar, arrays, and hashes may be abbreviated. Example output:

    $ perl -d:DumpTrace=4 littledemo.pl
    >>  littledemo.pl:1:[__top__]:
    >>>              @a = (1 .. 40);
    >>>>>            (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20, \
        21,22,23,24,25,26,27,...,40) = (1 .. 40);
    -------------------------------------------
    >>  littledemo.pl:2:[__top__]:
    >>>              $b = $a[4] + $a[5];
    >>>>             $b = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19, \
        20,21,22,23,24,25,26,27,...,40)[4];
    >>>>>            5 = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19, \
        20,21,22,23,24,25,26,27,...,40)[4];
    -------------------------------------------

=item C<$Devel::DumpTrace::TRACE> == 5

Like C<$TRACE> 4, but long scalars, arrays, and hashes are B<not> abbreviated.

    $ perl -I. -d:DumpTrace=5 littledemo.pl
    >>  littledemo.pl:1:[__top__]:
    >>>              @a = (1 .. 40);
    >>>>>            (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21, \
        22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40) = (1 .. 40);
    -------------------------------------------
    >>  littledemo.pl:2:[__top__]:
    >>>              $b = $a[4] + $a[5];
    >>>>             $b = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19, \
        20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40)[4];
    >>>>>            5 = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19, \
        20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40)[4];

=back

As these demos suggest, you can pass the C<$TRACE> variable through the
command line using the syntax C<< -d:DumpTrace=I<level> >>. In place of a
number, you may also use the keywords C<quiet> or C<verbose> which will
set the C<$TRACE> variable to 1 and 5, respectively.

By default C<Devel::DumpTrace> does not evaluate statements in any "system"
modules, which are defined as any module from a file that resides under
an absolute path in your system's C<@INC> list of directories. If the
C<$TRACE> variable is set to a value larger than 100, then this module
B<will> drill down into such modules. See also L</"EXCLUDE_PKG"> and
L</"INCLUDE_PKG"> for another way to exercise control over what packages
this module will explore.

For convenience, the C<$Devel::DumpTrace::TRACE> variable is aliased to
the C<$Devel::Trace::TRACE> variable. This way you can enable settings
in your program that can be used by both L<Devel::Trace|Devel::Trace>
and C<Devel::DumpTrace>.

=head2 C<$Devel::DumpTrace::DUMPTRACE_FH>

By default, all output from the C<Devel::DumpTrace> module
is written to standard error. This behavior can be changed
by setting C<$Devel::DumpTrace::DUMPTRACE_FH> to the desired
I/O handle:

    BEGIN {
       # if Devel::DumpTrace is loaded, direct output to xtrace-output.txt
       if ($INC{'Devel/DumpTrace.pm'}) {
          open $Devel::DumpTrace::DUMPTRACE_FH, '>', '/path/xtrace-output.txt';
       }
    }

The output stream for the C<Devel::DumpTrace> module can also be controlled
with the environment variable C<DUMPTRACE_FH>. If this variable is set
to C<STDOUT>, then output will be directed to standard output. If this
variable contains another value that looks like a filename, this module
will open a file with that name and write the trace output to that file.

B<< Backwards-incompatible change: >> in v0.06, this variable was called 
C<XTRACE_FH>.

=head2 C<$Devel::DumpTrace::ARRAY_ELEM_SEPARATOR = ','>

=head2 C<$Devel::DumpTrace::HASH_ENTRY_SEPARATOR = ';'>

=head2 C<< $Devel::DumpTrace::HASH_PAIR_SEPARATOR = '=>' >>

The C<Devel::DumpTrace> module uses the preceding default values as delimiters
when creating string representations of arrays, hashes, and array/hash
references. If you wish to use different delimiters for whatever reason
(maybe your arrays have a lot of elements with commas in them),
you can change these values.

=head2 C<< $Devel::DumpTrace::XEVAL_SEPARATOR = ':' >>

In normal (non-verbose) mode, C<Devel::DumpTrace> will display this token
between the name of a variable and its value (e.g., C<$c:23>). The
default token is a colon (C<:>), but you may change it by changing
the value of the variable C<$Devel::DumpTrace::XEVAL_SEPARATOR>.

=head2 %Devel::DumpTrace::EXCLUDE_PKG, %Devel::DumpTrace::INCLUDE_PKG

Sets of packages that this module will never/always explore.
These settings take precedence over the setting of the
C<$Devel::DumpTrace::TRACE> variable, and the settings of
C<%Devel::DumpTrace::INCLUDE_PKG> take precendence over the settings
of C<%Devel::DumpTrace::EXCLUDE_PKG> (that is, a package that is
specified in both C<%EXCLUDE_PKG> and C<%INCLUDE_PKG> will
be I<included>).

=head1 CONFIGURATION AND ENVIRONMENT

C<Devel::DumpTrace> uses the C<DUMPTRACE_FH> and C<DUMPTRACE_LEVEL>
environment variables to configure some package variables.
See L</"VARIABLES">.
The C<DUMPTRACE_NOPPI> variable can be set to force this module
to use the basic code parser and to ignore the L<PPI|PPI>-based
version.

B<< This is an incompatible change from v0.06, which recognized 
the vars C<XTRACE_FH> and C<XTRACE_LEVEL>. >>

=head1 INCOMPATIBILITIES

None known.

=head1 EXPORT

Nothing is or can be exported from this module.

=head1 DIAGNOSTICS

Nothing interesting.

=head1 DEPENDENCIES

C<Devel::DumpTrace> requires the L<PadWalker|PadWalker>
and L<Scalar::Util|Scalar::Util> modules.

=head1 BUGS AND LIMITATIONS

=head2 Parser limitations

Some known cases where the output of this module will
be incorrect or misleading include:

=head3 Multiple statements on one line

    $b = 7;
    $a=4; $b=++$a;
    >>>>> 4 = 4; 7 = 4;
    $a=4; $b=++$a;
    >>>>> 5 = 4; 5 = 5;

All statements on a line are evaluated, not just the statement
currently being executed.

=head3 Statements with chained assignments; complex assignment expressions

    ($a,$b) = ('','bar');
    $a = $b = 'foo';
    >>>>> 'foo' = 'bar' = 'foo';

    $rin=$ein=3;
    >>    select $rout=$in,undef,$eout=$ein,0;
    >>>   select $rout=3,undef,undef=3,0;
    >>>>> select 3=3,undef,undef=3,0;

Everything to the right of the I<first> assignment operator in a
statement is evaluated I<before> the statement is executed.

=head3 Multiple lines for one statement

    $a = ($b + $c                # ==> oops, all this module sees is
         + $d + $e);             #     $a = ($b + $c

Only the first line of code in an expression is evaluated.

=head3 Displayed value of @_ variable is unreliable

The displayed value of C<@_> inside a subroutine is subject to
some of the issues described in L<perlfunc/"caller">:

    ... be aware that setting @DB::args is best effort, intended for
    debugging or generating backtraces, and should not be relied upon
    ... a side effect of the current implementation means that effects
    of shift @_ can normally be undone (but not pop @_ or other splicing,
    and not if a reference to @_ has been taken, and subject to the caveat
    about reallocated elements), so @DB::args is actually a hybrid of the
    current state and initial state of @_ . Buyer beware.

That is, the displayed value of C<@_> inside a subroutine may be
corrupted. Different versions of Perl may have different behavior.

=head2 Basic parser limitations

This distribution ships with a L<PPI|PPI>-based parser
and a more basic parser that will be used if C<PPI> is not
available (or if you explicitly request to use the basic
parser). This parser is quite crude compared to the PPI-based
parser, and suffers from these additional known issues:

=head3 String literals that contain variable names

    print STDERR "\$x is $x\n";  # ==> print STDERR "\4 is 4\n";
    $email = 'mob@cpan.org';     # ==> $email = 'mob().org'

The parser is not sophisticated enough to tell whether a sigil is
inside non-interpolated quotes.

=head3 Implicit C<$_>, C<@_>

It would be nice if this parser could detect when Perl was
implicitly using some variables and display the implicit variable.

    /expression/;        # actually  $_ =~ /expression/
    my $self = shift;    # actually  my $self = shift(@_);

That is not currently a capability of this module.

=head3 Special Perl variables are not recognized

    $a = join $/, 'foo', 'bar';  # ==> $a = join $/, 'foo', 'bar'

Special variables with pure alphanumeric names like C<@ARGV>, C<$_>,
and C<$1> will still be interpolated. I<Do see>
L<perlfunc/"caller"> I<for some important caveats about how>
C<@_> I<is represented by this module>.

For some of these limitations, there are easy workarounds
(break up chained assignments, put all statements on separate lines, etc.)
if you think the extra information provided by this module is worth the
effort to make your code more friendly for this module.

=head2 Other bugs or feature requests

Please report any other bugs or feature requests to
C<bug-Devel-DumpTrace at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Devel-DumpTrace>.
I will be notified, and then you'll automatically be notified of
progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Devel::DumpTrace

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Devel-DumpTrace>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Devel-DumpTrace>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Devel-DumpTrace>

=item * Search CPAN

L<http://search.cpan.org/dist/Devel-DumpTrace/>

=back

=head1 SEE ALSO

L<dumpvar.pl|perl5db.pl>, as used by the Perl debugger.

L<Devel::Trace|Devel::Trace>, L<PadWalker|PadWalker>.

L<Devel::DumpTrace::PPI|Devel::DumpTrace::PPI> is part of this 
distribution and provides similar functionality using L<PPI|PPI> 
to parse the source code.

L<Devel::TraceVars|Devel::TraceVars> is a very similar effort to
C<Devel::DumpTrace>, but this
module handles arrays, hashes, references, objects, lexical C<our>
variables, and addresses more edge cases.

L<Tie::Trace|Tie::Trace> provides facilities to watch the values
of specific variables, including stack trace information about
where and how the variables values were changed.

=head1 AUTHOR

Marty O'Brien, E<lt>mob at cpan.orgE<gt>

=head1 LICENSE AND COPYRIGHT

Copyright 2010-2011 Marty O'Brien.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut
