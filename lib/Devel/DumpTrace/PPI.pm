package Devel::DumpTrace;          ## no critic (FilenameMatchesPackage)

use Devel::DumpTrace;
use PadWalker;
use Scalar::Util;
use Text::Shorten;
use Carp;
use strict;
use warnings;
no warnings 'redefine';
local $| = 1;

croak "Devel::DumpTrace::PPI may not be used when \$Devel::DumpTrace::NO_PPI ",
  "is set (Did you load 'Devel::DumpTrace::noPPI'?\n"
  if $Devel::DumpTrace::NO_PPI;
eval {use PPI;1}
  or croak "PPI not installed. Can't use Devel::DumpTrace::PPI module";

our $VERSION = '0.08';
our $IMPLICIT_ = 1;

# built-in functions that may use $_ implicitly
my %implicit_
  = (abs=>1, alarm=>1, chomp=>1, chop=>1, chr=>1, chroot=>1, cos=>1,
     defined=>1, eval=>1, exp=>1, glob=>1, hex=>1, int=>1, lc=>1,
     lcfirst=>1, length=>1, log=>1, lstat=>1, mkdir=>1, oct=>1, ord=>1,
     pos=>1, print=>1, quotemeta=>1, readlink=>1, readpipe=>1, ref=>1,
     require=>1, reverse=>1, rmdir=>1, sin=>1, split=>1, sqrt=>1, stat=>1,
     study=>1, uc=>1, ucfirst=>1, unlink=>1, unpack=>1,);
my (@deferred, $deferred_line, $deferred_pkg);

# for persisting the PPI documents we create
my (%ppi_src, %ppi_doc);

sub Devel::DumpTrace::PPI::import {
  my @arg = @_;
  return Devel::DumpTrace::import(@arg);
}

# Overrides &get_source in Devel/DumpTrace.pm
sub get_source {
  my ($file, $line) = @_;

  if ($file eq '-e' || $file =~ /^\(eval \d+\)\[/) {
    my $source = do {
      no strict 'refs';                    ## no critic (NoStrict)
      \@{"::_<$file"}
    };
    my $code = $source->[$line];
    my $doc = $ppi_doc{"$file:$line"} = PPI::Document->new(\$code);

    $ppi_src{$file}[$line] = [ $doc->elements ];

  } elsif (!defined $ppi_src{$file}) {

    my $doc = $ppi_doc{$file} = PPI::Document->new($file);
    $doc->index_locations;

    # find every separate statement in the document
    # and store by its line number.

    my $elements = $doc->find('PPI::Statement');

    # there may be more than one distinct statement per line ($a=4; $b=6;)
    # but statements that are children of other statements should not
    # be included ...

    ELEMENT: foreach my $element (@{$elements}) {

      my $_line = $element->line_number;
      $ppi_src{$file}[$_line] ||= [];

      # don't include this element on this line if any of its
      # ancestors are included in this line
      my $parent = $element->parent;
      while ($parent) {
	next ELEMENT if $parent->line_number == $_line;
	$parent = $parent->parent;
      }

      # 0.07:what whitespace and comments are there between the last code token
      #       of a statement and a ';' structure element? Remove them if there
      #       is more than one.
      my @tokens = $element->tokens();
      if (@tokens > 3 && ref($tokens[-1]) eq 'PPI::Token::Structure' && $tokens[-1] eq ';') {

	my $j = -2;
	while (defined($tokens[$j]) 
	       && (ref($tokens[$j]) eq 'PPI::Token::Whitespace' 
		   || ref($tokens[$j]) eq 'PPI::Token::Comment'
		   || ref($tokens[$j]) eq 'PPI::Token::POD')) {
	  $j--;
	}
	if ($j < -3 && defined($tokens[$j])) {

	  for my $k ($j+1 .. -1) {
	    print STDERR "Token $k $tokens[$k]: DELETED\n";
	    $tokens[$k]->delete();
	  }
	}
      }

      if (ref($element) eq 'PPI::Statement::Compound') {
	my ($d1, $d2) = (0,0);
	my @zrc = _get_source($file, $_line, $element, $d1, $d2);
	push @{$ppi_src{$file}[$_line]},
	  bless { children => [@zrc] }, ref $element;
      } else {
	push @{$ppi_src{$file}[$_line]}, $element;
      }
    }
  } elsif (!defined @{$ppi_src{$file}[$line]}) {
    my $source = do {
      no strict 'refs';                    ## no critic (NoStrict)
      \@{"::_<$file"}
    };
    my $code = $source->[$line];
    my $doc = $ppi_doc{"$file:$line"} = PPI::Document->new(\$code);
    $ppi_src{$file}[$line] = [ $doc->elements ];
  }
  return \@{$ppi_src{$file}[$line]};
}

use constant ANOTHER_LINE => 3;
use constant BLOCK_START  => 4;

# extract source from a compound statement that goes with the
# specified line. This involves removing tokens that appear on
# other lines AFTER a block opening ("{") has been observed.
sub _get_source {
  my ($file, $line, $node, $another_line, $block_start, @src) = @_;
  my @children = $node->elements;

  for my $element (@children) {
    $_[ANOTHER_LINE] += $element->line_number != $line;
    last if $_[ANOTHER_LINE] && $_[BLOCK_START];
    if ($element->first_token ne $element) {
      my @zrc = _get_source($file, $line, $element, $_[ANOTHER_LINE], $_[BLOCK_START]);
      push @src, bless { children=>[@zrc] }, ref $element;
      #@src = _get_source($file, $line, $element, $_[ANOTHER_LINE], $_[BLOCK_START], @src);
    } else {
      push @src, $element;
    }
    if (ref $element eq 'PPI::Token::Structure' && $element eq '{') {
      $_[BLOCK_START]++;
    }
  }
  return @src;
}

# Overrides &evaluate_and_display_line in Devel/DumpTrace.pm
sub evaluate_and_display_line {
  my ($statements, $pkg, $file, $line) = @_;

  if (ref $statements ne 'ARRAY') {
    my $doc = PPI::Document->new(\$statements);
    $ppi_doc{"$file:$line"} = $doc;
    $statements = [$doc->elements];
  }

  my $style = _display_style();

  my $code = join '', map { "$_" } @{$statements};
  chomp $code;
  $code .= "\n";
  $code =~ s/\n(.)/\n\t\t $1/g;

  my $fh = $Devel::DumpTrace::DUMPTRACE_FH;
  if ($style > DISPLAY_TERSE) {
    print {$fh} ">>    $file:$line:\n";
    print {$fh} ">>>   \t\t $code";
  }

  # recursive preval calls will increase the depth levels
  local $Devel::DumpTrace::DB_ARGS_DEPTH = 4;

  my @preval = ();
  for my $s (@{ $statements }) {
    push @preval, preval($s, $style);
  }
  my $xcode = join '', @preval;
  chomp $xcode;
  $xcode .= "\n";
  $xcode =~ s/\n(.)/\n\t\t $1/g;

  if ($style >= DISPLAY_GABBY && $xcode ne $code) {
    print {$fh} ">>>>  \t\t $xcode";
  }
  for my $preval (@preval) {
    if (ref $preval) {
      @deferred = @preval;
      $deferred_pkg = $pkg;
      if ($style == DISPLAY_TERSE) {
	$deferred_line = "$line:$file";
      }
      last;
    }
  }
  if (@deferred == 0) {
    if ($style >= DISPLAY_GABBY) {
      print {$fh} $Devel::DumpTrace::SEPARATOR;
    } else {
      print {$fh} ">>>   $file:$line:\t$xcode";
    }
  }
  return;
}

# any elements that appear AFTER the last assignment operator
# are evaluated and tokenized.
# McCabe score for preval: 60   :-(
sub preval {
  my ($statement,$style,$pkg) = @_;
  if (ref($statement) =~ /PPI::Token/) {
    return map {"$_"} $statement->tokens;
  }
  $Devel::DumpTrace::DB_ARGS_DEPTH++;
  my @e = $statement->elements;

  # look for implicit use of $_. XXX - would it be better to
  #    put this functionality in get_source? somewhere else?
  if ($IMPLICIT_) {

    for (my $i=0; $i<@e; $i++) {

      #
      # /pattern/   means  $_ =~ /pattern/
      #
      if (ref($e[$i]) =~ /^PPI::Token::Regexp/) {
	my $j = $i-1;
	while ($j>=0 && ref $e[$j] eq 'PPI::Token::Whitespace') {
	  $j--;
	}
	if ($j < 0 || ref $e[$j] ne 'PPI::Token::Operator'
	    || ($e[$j] ne '=~' && $e[$j] ne '!~')) {

	  splice @e, $i, 0,
	    bless({content=>'$_'}, 'PPI::Token::Magic'),
	    bless({content=>'=~'}, 'PPI::Token::Operator');
	}
      }

      #
      # print;   means   print $_;   for print and a lot of other builtins
      #
      if (ref $e[$i] eq 'PPI::Token::Word' && defined $implicit_{"$e[$i]"}) {
	my $j = $i + 1;
	while ($j <= @e && ref $e[$j] eq 'PPI::Token::Whitespace') {
	  $j++;
	}
	if ($j >= @e || ref $e[$j] eq 'PPI::Token::Operator'
	    || ref $e[$j] eq 'PPI::Token::Structure') {

	  if ($e[$i] eq 'split') {
	    # naked  split  is parsed as  split /\s+/, $_
	    splice @e, $i+1, 0,
	      bless({content=>' '}, 'PPI::Token::Whitespace'),
	      bless({content=>'m/\\s+/'}, 'PPI::Token::Regexp::Match'),
	      bless({content=>','}, 'PPI::Token::Operator'),
	      bless({content=>'$_', _DEFER => 1}, 'PPI::Token::Magic');
	  } else {
	    splice @e, $i+1, 0,
	      bless({content=>' '}, 'PPI::Token::Whitespace'),
	      bless({content=>'$_', _DEFER => 1}, 'PPI::Token::Magic');
	  }
	}
      }

      #
      # -X  means  -X $_    (except for -t)
      #
      if (ref $e[$i] eq 'PPI::Token::Operator'
	  && $e[$i] =~ /^-[a-su-zA-Z]$/) {
	my $j = $i + 1;
	while ($j <= @e && ref($e[$j]) eq 'PPI::Token::Whitespace') {
	  $j++;
	}
	if ($j >= @e || ref $e[$j] eq 'PPI::Token::Operator'
	   || ref $e[$j] eq 'PPI::Token::Structure') {

	  splice @e, $i+1, 0,
	    bless({content=>' '}, 'PPI::Token::Whitespace'),
	    bless({content=>'$_'}, 'PPI::Token::Magic');
	}
      }

      #
      # implicit $_ in a  for (LIST), foreach (LIST) construction
      #
      if ($e[$i] eq 'PPI::Token::Word'
	  && ($e[$i] eq 'for' || $e[$i] eq 'foreach')) {
	my $j = $i+1;
	while ($j<@e && ref($e[$j]) eq 'PPI::Token::Whitespace') {
	  $j++;
	}
	if ($j<@e && ref($e[$j]) eq 'PPI::Structure::List') {
	  splice @e, $i+1, 0,
	    bless({content=>' '}, 'PPI::Token::Whitespace'),
	    bless({content=>'$_'}, 'PPI::Token::Magic');
	}
      }
    }

    #
    # look for use of implicit @_/@ARGV with shift/pop
    #
    for (my $i=0; $i<@e; $i++) {
      if (ref($e[$i]) eq 'PPI::Token::Word'
	  && ($e[$i] eq 'shift' || $e[$i] eq 'pop')) {

	my $j = $i + 1;
	while ($j <= @e && ref($e[$j]) eq 'PPI::Token::Whitespace') {
	  $j++;
	}
	if ($j >= @e || ref($e[$j]) eq 'PPI::Token::Operator'
	   || ref($e[$j]) eq 'PPI::Token::Structure') {

	  # found naked pop/shift. Determine if we are inside a sub
	  # so we know whether to apply @ARGV or @_.
	  my $n = 0;
	  my $xp = 0;
	  while (my @p = caller($n++)) {
	    $xp += $p[CALLER_PKG] !~ /^Devel::DumpTrace/ &&
	           $p[CALLER_SUB] !~ /^\(eval/;
	  }
	  if ($xp >= 2) { # inside sub, manip @_
	    splice @e, $i+1, 0,
	      bless({content=>' '}, 'PPI::Token::Whitespace'),
	      bless({content=>'@_'}, 'PPI::Token::Magic');
	  } else {        # not inside sub, manip @ARGV
	    splice @e, $i+1, 0,
	      bless({content=>' '}, 'PPI::Token::Whitespace'),
	      bless({content=>'@ARGV'}, 'PPI::Token::Symbol');
	  }
	}
      }
    }

    # TODO -  ; split /pattern/, $var   means   @_ = split /pattern/, $var
    # This one is tricky. @_ is used in both void and *scalar* context, so
    #
    #   split  construction                  implicit @_ load?
    #       split /pattern/, var                 YES
    #       @z = split /pattern/, var            NO
    #       $z = split /pattern/, var            YES
    #       $z += split /pattern/, var           YES
    #       push @z, split /pattern/;            NO
    #       (split /pattern/) {...}              NO
    #       $z = [ split /pattern/ ]             NO
    #       $z = { split /pattern/ }             NO
    #       $z = do { split /pattern/ }          YES
    #       \& { split /pattern/ }               YES
    #       sub { split /pattern/ }              YES
  }

  # find last assignment operator in this expression, if any.
  my $lao_index = 0;
  for my $i (0 .. $#e) {
    if (ref $e[$i] eq 'PPI::Token::Operator') {
      if ($e[$i]->is_assignment_operator) {
	$lao_index = $i;
      } elsif (ref $e[$i] eq 'PPI::Token::Regexp::Substitute') {
	if (0) {
	  # Should eval for  $var =~ s/abc/def/   also be deferred?
	  # No. Usually, $var (or $1,$2,...) will be seen shortly after
	  # such an expression. TODO - make this configurable.
	  $lao_index = $i;
	}
      }
    }
  }

  # evaluate any PPI::Token::Symbol elements after element $z
  # tokenize other PPI::Token::* elements
  # pass other elements back to &preval recursively
  for my $i ($lao_index .. $#e) {
    if (ref $e[$i] eq 'PPI::Token::Symbol') {
      next if $e[$i]->{_DEFER};
      perform_variable_substitution(@e, $i, $style, $pkg);
      if ($i > 0 && ref($e[$i-1]) eq 'PPI::Token::Cast') {
	if ($e[$i-1] eq '@' && $e[$i] =~ /^\[(.*)\]$/) {

	  # @$a => @[1,2,3]   should render as   @$a => (1,2,3)

	  $e[$i-1] = '';
	  $e[$i] = '(' . substr($e[$i],1,-1) . ')';
	} elsif ($e[$i-1] eq '%' && $e[$i] =~ /^\{(.*)\}$/) {

	  # render  %$a  as  ('a'=>1;'b'=>2) , not  %{'a'=>1;'b'=>2}

	  $e[$i-1] = '';
	  $e[$i] = '(' . substr($e[$i],1,-1) . ')';
	}
      }
    } elsif (ref $e[$i] eq 'PPI::Token::Magic') {
      next if $e[$i]->{_DEFER};
      perform_variable_substitution(@e, $i, $style, '<magic>');
    } elsif (ref($e[$i]) =~ /PPI::Token/) {
      $e[$i] = "$e[$i]" if ref($e[$i]) ne 'PPI::Token::Cast';
    } else {
      $e[$i] = [ preval($e[$i],$style,$pkg) ];
    }
  }
  my @output = map { ref($_) eq 'ARRAY' ? @{$_} : $_ } @e;
  $Devel::DumpTrace::DB_ARGS_DEPTH--;
  return @output;
}

# Overrides &handle_deferred_output in Devel/DumpTrace.pm
sub handle_deferred_output {
  if (@deferred) {
    my $fh = $Devel::DumpTrace::DUMPTRACE_FH;
    my @e = @deferred;
    @deferred = ();
    my $style = _display_style();
    for my $i (0 .. $#e) {
      if (ref $e[$i] eq 'PPI::Token::Symbol') {
	perform_variable_substitution(@e, $i, $style, $deferred_pkg);
      } elsif (ref $e[$i] eq 'PPI::Token::Magic') {
	perform_variable_substitution(@e, $i, $style, '<magic>');
      } elsif (ref $e[$i]) {
	$e[$i] = join '', $e[$i]->tokens();
      }
    }
    my $deferred_output = join '', @e;
    chomp $deferred_output;
    $deferred_output .= "\n";
    $deferred_output =~ s/\n(.)/\n\t\t $1/g;
    if (defined $deferred_line) {
      my ($line,$file) = split /:/, $deferred_line, 2;
      $deferred_line = undef;
      print {$fh} ">>>>> $file:$line:\t$deferred_output";
    } else {
      print {$fh} ">>>>>\t\t $deferred_output";
      print {$fh} $Devel::DumpTrace::SEPARATOR;
    }
  }
  return;
}

sub perform_variable_substitution {
  my $pkg = pop @_;
  my $style = pop @_;
  my $i = pop @_;

  my $sigil = substr $_[$i], 0, 1;
  return if $sigil eq '&' || $sigil eq '*';
  my $varname = substr $_[$i], 1;
  $varname =~ s/^\s+//;
  $varname =~ s/\s+$//;
  my $deref_op = '';
  my $index_op = '';

  my $j = $i+1;
  while ($j < @_ && ref($_[$j]) eq 'PPI::Token::Whitespace') {
    $j++;
  }
  if (ref($_[$j]) eq 'PPI::Token::Operator' && $_[$j] eq '->') {
    $deref_op = '->';
    $j++;
    while ($j < @_ && ref $_[$j] eq 'PPI::Token::Whitespace') {
      $j++;
    }
  }
  if (ref($_[$j]) =~ /^PPI::Structure::/) {
    my @t = $_[$j]->tokens();
    if ($t[0] eq '[') {
      $index_op = '[';
    } elsif ($t[0] eq '{') {
      $index_op = '{';
    }
  }

  $_[$i] = evaluate($sigil,$varname,$deref_op,$index_op, $pkg);
  $_[$i] =~ s/[\[\{]$//;
  $_[$i] =~ s/\-\>$//;
  if ($style < DISPLAY_GABBY) {
    $_[$i] = "$sigil$varname$Devel::DumpTrace::XEVAL_SEPARATOR" . $_[$i];
  }
  return $_[$i];
}

sub PPI::Token::Operator::is_assignment_operator {
  my $op = $_[0]->{content};
  for my $aop (qw(= += -= *= /= %= &= |= ^= .= x=
		 **= &&= ||= //= <<= >>=)) {
    return 1 if $op eq $aop;
  }
  return;
}

1;

__END__

=head1 NAME

Devel::DumpTrace::PPI - PPI-based version of Devel::DumpTrace

=head1 VERSION

0.08

=head1 SYNOPSIS

    perl -d:DumpTrace::PPI demo.pl
    >>>>> demo.pl:3:        $a:1 = 1;
    >>>>> demo.pl:4:        $b:3 = 3;
    >>>>> demo.pl:5:        $c:23 = 2 * $a:1 + 7 * $b:3;
    >>>>> demo.pl:6:        @d:(1,3,26) = ($a:1, $b:3, $c:23 + $b:3);

    perl -d:DumpTrace::PPI=verbose demo.pl
    >>   demo.pl:3:
    >>>              $a = 1;
    >>>>>            1 = 1;
    ------------------------------------------
    >>   demo.pl:4:
    >>>              $b = 3;
    >>>>>            3 = 3;
    ------------------------------------------
    >>   demo.pl:5:
    >>>              $c = 2 * $a + 7 * $b;
    >>>>             $c = 2 * 1 + 7 * 3;
    >>>>>            23 = 2 * 1 + 7 * 3;
    ------------------------------------------
    >>   demo.pl:6:
    >>>              @d = ($a, $b, $c + $b);
    >>>>             @d = (1, 3, 23 + 3);
    >>>>>            (1,3,26) = (1, 3, 23 + 3);
    ------------------------------------------

=head1 DESCRIPTION

C<Devel::DumpTrace::PPI|"Devel::DumpTrace::PPI"> is a near drop-in replacement
to L<Devel::DumpTrace|"Devel::DumpTrace"> that uses the L<PPI|"PPI module">
for parsing the source code.
PPI overcomes some of the limitations of the original C<Devel::DumpTrace>
parser, including

=over 4

=item * handling statements with chained assignments of complex assignment
expressions

  $ perl -d:DumpTrace=verbose -e '$a=$b[$c=2]="foo"'
  >>  -e:1:
  >>>              $a=$b[$c=2]="foo"
  >>>>             $a=()[undef=2]="foo"
  >>>>>            'foo'=()[undef=2]="foo"
  -------------------------------------------

  $ perl -d:DumpTrace::PPI=verbose -e '$a=$b[$c=2]="foo"'
  >>   -e:1:
  >>>              $a=$b[$c=2]="foo"
  >>>>>            'foo'=(undef,undef,'foo')[$c=2]="foo"
  ------------------------------------------

=item * multi-line statements

  $ cat multiline.pl
  $b = 4;
  @a = (1 + 2,
        3 + $b);

    $ perl -d:DumpTrace=verbose multiline.pl
    >>  multiline.pl:1:
    >>>              $b = 4;
    >>>>>            4 = 4;
    -------------------------------------------
    >>  multiline.pl:2:
    >>>              @a = (1 + 2,
    >>>>>            (3,7) = (1 + 2,
    -------------------------------------------

    $ perl -d:DumpTrace::PPI=verbose multiline.pl
    >>   multiline.pl:1:
    >>>              $b = 4;
    >>>>>            4 = 4;
    ------------------------------------------
    >>   multiline.pl:2:
    >>>              @a = (1 + 2,
                           3 + $b);
    >>>>             @a = (1 + 2,
                           3 + 4);
    >>>>>            (3,7) = (1 + 2,
                           3 + 4);
    ------------------------------------------

=item * string literals with variable names

    $ perl -d:DumpTrace=verbose -e '$email = q/mob@cpan.org/'
    >>  -e:1:
    >>>              $email = q/mob@cpan.org/
    >>>>             $email = q/mob().org/
    >>>>>            "mob\@cpan.org" = q/mob().org/
    -------------------------------------------

    $ perl -d:DumpTrace::PPI=verbose -e '$email = q/mob@cpan.org/'
    >>   -e:1:
    >>>              $email = q/mob@cpan.org/
    >>>>>            "mob\@cpan.org" = q/mob@cpan.org/
    ------------------------------------------

=item * Better recognition of Perl's magic variables

    $ perl -d:DumpTrace=verbose -e '$"="\t";'  -e 'print join $", 3, 4, 5'
    >>  -e:1:
    >>>              $"="\t";
    >>>>>            $"="\t";
    -------------------------------------------
    >>  -e:2:
    >>>              print join $", 3, 4, 5
    -------------------------------------------
    3       4       5

    $ perl -d:DumpTrace::PPI=verbose -e '$"="\t";' -e 'print join $", 3, 4, 5'
    >>   -e:1:
    >>>              $"="\t";
    >>>>>            "\t"="\t";
    ------------------------------------------
    >>   -e:2:
    >>>              print join $", 3, 4, 5
    >>>>             print join "\t", 3, 4, 5
    ----------------------------------------------
    3       4       5

=item * Can insert implicitly used C<$_>, C<@_>, C<@ARGV> variables

C<$_> is often used as an implicit target of regular expressions
or an implicit argument to many standard functions. 
C<@_> and C<@ARGV> are often implicitly used as arguments to
C<shift> or C<pop>. This module can identify some places where
these variables are used implicitly and include their values
in the trace output.

    $ perl -d:DumpTrace::PPI=verbose -e '$_=pop;' \
          -e 'print m/hello/ && sin' hello

    >>    -e:1:
    >>>              $_=pop;
    >>>>             $_=pop ('hello');
    >>>>>            'hello'=pop ('hello');
    -------------------------------------------
    >>    -e:2:
    >>>              print m/hello/ && sin
    >>>>             print 'hello'=~m/hello/ && sin $_
    0>>>>>           print 'hello'=~m/hello/ && sin 'hello'
    -------------------------------------------

=back

The PPI-based parser has more overhead than the simpler parser from
L<Devel::DumpTrace|"Devel::DumpTrace"> (benchmarks forthcoming),
so you may not want to always favor
C<Devel::DumpTrace::PPI|"Devel::DumpTrace::PPI"> over
C<Devel::DumpTrace|"Devel::DumpTrace">.
Plus it requires L<PPI|"PPI"> to be installed.

See L<Devel::DumpTrace|"Devel::DumpTrace"> for far more information
about what this module is supposed to do, including the variables
and configuration settings.

=head1 SUBROUTINES/METHODS

None to worry about.

=head1 EXPORT

Nothing is or can be exported from this module.

=head1 DIAGNOSTICS

None

=head1 CONFIGURATION AND ENVIRONMENT

Like L<Devel::DumpTrace|"Devel::DumpTrace">, this module also reads
the C<DUMPTRACE_FH> and
C<DUMPTRACE_LEVEL> environment variables.
See C<Devel::DumpTrace> for
details about how to use these variables.

=head1 DEPENDENCIES

L<PPI|"PPI"> for understanding the structure of your Perl script.

L<PadWalker|"PadWalker"> for arbitrary access to lexical variables.

L<Scalar::Util|"Scalar::Util"> for the reference identification
convenience methods.

L<Text::Shorten|"Text::Shorten"> (bundled with this distribution)
for abbreviating long output, when desired.

=head1 INCOMPATIBILITIES

None known.

=head1 BUGS AND LIMITATIONS

See L<Devel::DumpTrace/"SUPPORT"> for support and feature request information.
Report issues for this module with the C<Devel-DumpTrace> distribution.

=head1 AUTHOR

Marty O'Brien, E<lt>mob at cpan.orgE<gt>

=head1 LICENSE AND COPYRIGHT

Copyright 2010-2011 Marty O'Brien.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut
