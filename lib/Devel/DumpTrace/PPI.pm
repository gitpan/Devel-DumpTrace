package Devel::DumpTrace;          ## no critic (FilenameMatchesPackage)

use Devel::DumpTrace;
use PadWalker;
use Scalar::Util;
use Text::Shorten;
use Carp;
use strict;
use warnings;

# functions in this file that override functions in Devel/DumpTrace.pm

*get_source = *get_source_PPI;
*evaluate_and_display_line = *evaluate_and_display_line_PPI;
*handle_deferred_output = *handle_deferred_output_PPI;


local $| = 1;

croak "Devel::DumpTrace::PPI may not be used when \$Devel::DumpTrace::NO_PPI ",
  "is set (Did you load 'Devel::DumpTrace::noPPI'?\n"
  if $Devel::DumpTrace::NO_PPI;
eval {use PPI;1}
  or croak "PPI not installed. Can't use Devel::DumpTrace::PPI module";

our $VERSION = '0.09';
our $IMPLICIT_ = 1;

# built-in functions that may use $_ implicitly
my %implicit_
  = (abs=>1, alarm=>1, chomp=>1, chop=>1, chr=>1, chroot=>1, cos=>1,
     defined=>1, eval=>1, exp=>1, glob=>1, hex=>1, int=>1, lc=>1,
     lcfirst=>1, length=>1, log=>1, lstat=>1, mkdir=>1, oct=>1, ord=>1,
     pos=>1, print=>1, quotemeta=>1, readlink=>1, readpipe=>1, ref=>1,
     require=>1, reverse=>1, rmdir=>1, sin=>1, split=>1, sqrt=>1, stat=>1,
     study=>1, uc=>1, ucfirst=>1, unlink=>1, unpack=>1,);

# for persisting the PPI documents we create
my (%ppi_src, %ppi_doc);

my $last_file_sub_displayed = '';
my $last_file_line_displayed = '';

sub Devel::DumpTrace::PPI::import {
  my @arg = @_;
  foreach my $PPI_package (grep { m{^PPI[/.]} } keys %INC) {
    $PPI_package =~ s/\.pm$//;
    $PPI_package =~ s{/}{::}g;
    $Devel::DumpTrace::EXCLUDE_PKG{$PPI_package} = 1;
  }
  $Devel::DumpTrace::EXCLUDE_PKG{"Carp::Always"} = 1;
  return Devel::DumpTrace::import(@arg); # XXX - goto?
}

# Overrides &get_source in Devel/DumpTrace.pm
sub get_source_PPI {
  my ($file, $line) = @_;

  if ($file eq '-e' || $file eq '-' || $file =~ /^\(eval \d+\)\[/) {
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
	# $element->{'__FILE__'} = $file;

      my $_line = $element->line_number;
      $ppi_src{$file}[$_line] ||= [];

      # don't include this element on this line if any of its
      # ancestors are included in this line
      my $parent = $element->parent;
      while ($parent && ref($parent) ne 'PPI::Document') {
	my $parent_line = $parent->line_number;
	if (defined($parent_line) && $parent_line == $_line) {
	  next ELEMENT;
	}
	$parent = $parent->parent;
      }

      __remove_whitespace_and_comments_just_before_end_of_statement($element);
      __prepend_to_first_statement_in_for_block($element, $file);
      __prepend_to_first_statement_in_while_block($element, $file);
      __prepend_to_next_statements_in_ifelse_block($element, $file);

      if (ref($element) eq 'PPI::Statement::Compound') {
	my ($d1, $d2) = (0,0);
	my @zrc = _get_source($file, $_line, $element, $d1, $d2);
	push @{$ppi_src{$file}[$_line]},
	  bless { %$element, children => [@zrc] }, ref $element;

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
      my @zrc = _get_source($file, $line, $element,
			    $_[ANOTHER_LINE], $_[BLOCK_START]);
      push @src, bless { children=>[@zrc] }, ref $element;
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
sub evaluate_and_display_line_PPI {
  my ($statements, $pkg, $file, $line, $sub) = @_;

  if (ref $statements ne 'ARRAY') {
    my $doc = PPI::Document->new(\$statements);
    $ppi_doc{"$file:$line"} = $doc;
    $statements = [$doc->elements];
  }

  my $style = _display_style();
  my $code;
  my @s = @{$statements};

  # Many Perl flow control constructs are optimized to not
  # allow a breakpoint at each iteration of a loop or at each
  # condition evaluation of a complex if-elsif-else statement.
  # So there are times when, while evaluating the first statement
  # in a block, we also want to evaluate some other expressions
  # from the parent flow control structure.

  if (defined($statements->[0]{__FOR_LINE__})
      && $last_file_line_displayed ne $statements->[0]{__FOR_LINE__}) {

    my $ws = $style == DISPLAY_TERSE ? "\n\t\t\t" : " ";
    unshift @s, __new_token("FOR-UPDATE: {"), $statements->[0]{__CONTINUE__},
      __new_token(" } FOR-COND: {"), $statements->[0]{__CONDITION__},
      __new_token(" }" . $ws);

  } elsif (defined ($statements->[0]{__WHILE_LINE__})
	   && $last_file_line_displayed ne $statements->[0]{__WHILE_LINE__}) {
    my $ws = $style == DISPLAY_TERSE ? "\n\t\t\t" : " ";
    unshift @s, 
      __new_token($statements->[0]{__BLOCK_NAME__} . ": "), 
      $statements->[0]{__CONDITION__},
      __new_token(" " . $ws);
  } elsif (defined ($statements->[0]{__IF_LINE__})
	   && $last_file_line_displayed eq $statements->[0]{__IF_LINE__}) {

    my $ws = $style == DISPLAY_TERSE ? "\n\t\t\t" : " ";
    unshift @s, 
      @{$statements->[0]{__CONDITIONS__}}, 
      __new_token(" ". $ws);
  }

  $code = join '', map { "$_" } @s;
  chomp $code;
  $code .= "\n";
  $code =~ s/\n(.)/\n\t\t $1/g;

  my $fh = $Devel::DumpTrace::DUMPTRACE_FH;
  if ($style > DISPLAY_TERSE) {
    separate2();
    print {$fh} ">>    ", current_position_string($file,$line,$sub), "\n";
    print {$fh} ">>>   \t\t $code";
    $last_file_sub_displayed = "$file:$sub";
    $last_file_line_displayed = "$file:$line";
  }

  my $xcode;
  my @preval = ();

  # for a simple lexical declaration with no assignments,
  # don't evaluate the code:
  #           my ($a, @b, %c);
  #           our $XXX;
  # XXX - these expressions lifted from Devel::DumpTrace. Is that
  #       sufficient or should we analyze the PPI tokens?
  if ($code    =~ /^ \s* (my|our) \s*
                    [\$@%*\(] /x           # lexical declaration
      && $code =~ / (?<!;) .* ;
                    \s* (\# .* )? $/x   # single statement, single line
      && $code !~ /=/) {                # NOT an assignment

    $xcode = $code;

  } else {

    # recursive preval calls will increase the depth levels
    local $Devel::DumpTrace::DB_ARGS_DEPTH = 4;

    #for my $s (@{ $statements }) {
    for my $s (@s) {
      push @preval, preval($s, $style);
    }
    $xcode = join '', @preval;
  }

  chomp $xcode;
  $xcode .= "\n";
  $xcode =~ s/\n(.)/\n\t\t $1/g;

  if ($style >= DISPLAY_GABBY && $xcode ne $code) {
    print {$fh} ">>>>  \t\t $xcode";
  }
  my $deferred = 0;
  for my $preval (@preval) {
    if (ref $preval) {

      my ($pad_my, $pad_our) = get_pads();
      $deferred++;
      $Devel::DumpTrace::DEFERRED{"$sub : $file"} =
	{ EXPRESSION => [ @preval ],
	  PACKAGE    => $pkg,
	  MY_PAD     => $pad_my,
	  OUR_PAD    => $pad_our,
	  SUB        => $sub,
	  FILE       => $file,
	  LINE       => $line,
	  DISPLAY_FILE_AND_LINE => $style <= DISPLAY_TERSE,
	};
      last;
    }
  }
  if ($deferred == 0) {
    if ($style > DISPLAY_TERSE) {
      separate();
    } else {
      print {$fh} ">>>   ", 
	current_position_string($file,$line,$sub),
	"\t$xcode";
      $last_file_sub_displayed = "$file:$sub";
      $last_file_line_displayed = "$file:$line";
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
      if (defined($e[$i]) 
	  && defined(ref $e[$i])
	  && ref($e[$i]) eq 'PPI::Token::Word'
	  && defined $implicit_{"$e[$i]"}) {


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
sub handle_deferred_output_PPI {

  my ($sub, $file) = @_;
  my $deferred = delete $Devel::DumpTrace::DEFERRED{"$sub : $file"};
  return unless defined($deferred);

  my $fh = $Devel::DumpTrace::DUMPTRACE_FH;
  my @e = @{$deferred->{EXPRESSION}};
  my $undeferred_output = join '', @e;
  my $deferred_pkg = $deferred->{PACKAGE};
  $Devel::DumpTrace::pad_my = $deferred->{MY_PAD};
  $Devel::DumpTrace::pad_our = $deferred->{OUR_PAD};
  refresh_pads();
  #save_pads($deferred->{MY_PAD}, $deferred->{OUR_PAD});
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
  chomp($undeferred_output,$deferred_output);
  $undeferred_output .= "\n";
  $undeferred_output =~ s/\n(.)/\n\t\t $1/g;
  $deferred_output .= "\n";
  $deferred_output =~ s/\n(.)/\n\t\t $1/g;
  my $line = $deferred->{LINE};
  $file = $deferred->{FILE};
  $sub = $deferred->{SUB};
  if ($deferred->{DISPLAY_FILE_AND_LINE}
      || "$file:$sub" ne $last_file_sub_displayed) {

    if (_display_style() > DISPLAY_TERSE) {
      separate2();
      print {$fh} ">>>>  ", current_position_string($file,$line,$sub), "\n";
      print {$fh} ">>>> \t\t $undeferred_output";
      print {$fh} ">>>>>\t\t $deferred_output";
      separate();
    } else {
      print {$fh} ">>>>> ", 
	current_position_string($file,$line,$sub),
	  "\t$deferred_output";
    }
  } else {
    print {$fh} ">>>>>\t\t $deferred_output";
    separate();
  }
  $last_file_sub_displayed = "$file:$sub";
  $last_file_line_displayed = "$file:$line";
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

# 0.07: If a statement ends with whitespace and/or comments before the
#       ';' token, remove them for appearances sake.
sub __remove_whitespace_and_comments_just_before_end_of_statement {
  my $element = shift;
  my @tokens = $element->tokens();
  return if @tokens <= 3
    || ref($tokens[-1]) ne 'PPI::Token::Structure'
    || $tokens[-1] ne ';';

  my $j = -2;
  while (defined($tokens[$j]) 
	 && (ref($tokens[$j]) eq 'PPI::Token::Whitespace' 
	     || ref($tokens[$j]) eq 'PPI::Token::Comment'
	     || ref($tokens[$j]) eq 'PPI::Token::POD')) {
	  $j--;
	}
  if ($j < -3 && defined($tokens[$j])) {
    for my $k ($j+1 .. -1) {
      $tokens[$k]->delete();
    }
  }
}

# A C-style for-loop has the structure:
#     for (INIT ; CONDITION ; CONTINUE) BLOCK
#
# The CONDITION expression (and sometimes the CONTINUE) expression sure
# would be interesting to see while you are tracing through a program.
# Unfortunately, DB::DB will typically only get called at the very
# start of the loop.
#
# One workaround might be to prepend the for statement to the
# source code associated with the first statement in the BLOCK.
# That way, each time a new iteration starts, you would get
# the chance to observe the CONDITION and CONTINUE expressions.
#
sub __prepend_to_first_statement_in_for_block {

  # We expect a particular pattern of PPI code to describe the
  # "first statement in the block of a C-style for-loop":
  #
  # PPI::Statement::Compound                     $gparent
  #   PPI::Token::Word (for/foreach)
  #   zero of more PPI::Token::Whitespace
  #   PPI::Structure::For                        $for
  #     ...
  #   zero or more PPI::Token::Whitespace
  #   PPI::Structure::Block                      $parent
  #     zero or more PPI::Token::xxx
  #     PPI::Statement::xxx                      $element
  #


  my ($element, $file) = (@_);
  return if ref($element) !~ /^PPI::Statement/;
  my $parent = $element->parent;
  return if !defined($parent) || ref($parent) ne 'PPI::Structure::Block';

  my $gparent = $parent->parent;
  return if !defined($gparent) || ref($gparent) ne 'PPI::Statement::Compound';

  my @parent_elem = grep { ref($_) !~ /^PPI::Token/ } $parent->elements();
  return if $parent_elem[0] ne $element;

  my @gparent_elem = grep { ref($_) !~ /^PPI::Token/ } $gparent->elements();
  my $for = $gparent_elem[0];
  return if ref($for) ne 'PPI::Structure::For';
  return if @gparent_elem < 2 || $gparent_elem[1] ne $parent;

  # now what do we do with it ... ?
  # we want to _prepend_ the tokens^H^H^H^H^H elements of $element
  # with all the tokens in $gparent up to $parent, plus all
  # the tokens of $parent up to $element.
  my @old_elements = $element->elements();
  my @new_elements = ();

  foreach my $gparent_elem ($gparent->elements()) {

    last if $gparent_elem eq $parent;
    if ($gparent_elem eq $for) {

      my @for_statements 
	= grep { ref($_) =~ /^PPI::Statement/ } $for->elements();

      my $condition_statement = $for_statements[1]->clone();
      $element->{__CONDITION__} = $condition_statement;

      if (@for_statements > 2) {
	my $continue_statement = $for_statements[2]->clone();
	$element->{__CONTINUE__} = $continue_statement->clone();
      } else {
	$element->{__CONTINUE__} = __new_null_statement()->clone();
      }
      
      my $line = $for->line_number;
      $element->{__FOR_LINE__} = "$file:$line";
    }
  }
  return;
}

sub __prepend_to_first_statement_in_while_block {
  # We expect a particular pattern of PPI code to describe the
  # "first statement in the block of a C-style for-loop":
  #
  # PPI::Statement::Compound                     $gparent
  #   PPI::Token::Word (while/until)
  #   zero of more PPI::Token::Whitespace
  #   PPI::Structure::Condition                  $cond
  #     ...
  #   zero or more PPI::Token::Whitespace
  #   PPI::Structure::Block                      $parent
  #     zero or more PPI::Token::xxx
  #     PPI::Statement::xxx                      $element
  #

  my ($element, $file) = (@_);
  return if ref($element) !~ /^PPI::Statement/;
  my $parent = $element->parent;
  return if !defined($parent) || ref($parent) ne 'PPI::Structure::Block';

  my $gparent = $parent->parent;
  return if !defined($gparent) || ref($gparent) ne 'PPI::Statement::Compound';

  my @parent_elem = grep { ref($_) !~ /^PPI::Token/ } $parent->elements();
  return if $parent_elem[0] ne $element;

  my @gparent_elem = grep { ref($_) !~ /^PPI::Token/ } $gparent->elements();
  my $cond = $gparent_elem[0];
  return if ref($cond) ne 'PPI::Structure::Condition';
  return if @gparent_elem < 2 || $gparent_elem[1] ne $parent;

  my $cond_name = '';
  foreach my $gparent_elem ($gparent->elements()) {

    if (ref($gparent_elem) eq 'PPI::Token::Word' && $cond_name eq '') {
      $cond_name = "$gparent_elem";
    }

    last if $gparent_elem eq $parent;
    if ($gparent_elem eq $cond) {

      $element->{__BLOCK_NAME__} = uc ($cond_name || 'COND');
      $element->{__CONDITION__} = $cond->clone();
      my $line = $cond->line_number;
      $element->{__WHILE_LINE__} = "$file:$line";
      return;
    }
  }
  return;
}

# in a long chain of if/elsif/else blocks, 
# say if(COND1) BLOCK1 elsif(COND2) BLOCK2 elsif(COND3) BLOCK3 else BLOCK4,
# only the first condition (COND1) gets displayed in a trace. To get more
# useful trace output, prepend conditions to the first statement in
# each block to be displayed with the trace. That is, display
#   COND2 with the first statement in BLOCK2,
#   COND2 and COND3 with the first statement in BLOCK3, and
#   COND2 and COND3 with the first statement in BLOCK4.
# 
sub __prepend_to_next_statements_in_ifelse_block {
  my ($element, $file) = @_;
  return if ref($element) !~ /^PPI::Statement/;
  my $parent = $element->parent;
  return if !defined($parent) || ref($parent) ne 'PPI::Structure::Block';

  my $gparent = $parent->parent;
  return if !defined($gparent) || ref($gparent) ne 'PPI::Statement::Compound';

  my @parent_elem = grep { ref($_) !~ /^PPI::Token/ } $parent->elements();
  return if $parent_elem[0] ne $element;

  my @gparent_elem = grep { ref($_) !~ /^PPI::Token/ } $gparent->elements();
  my $cond = $gparent_elem[0];
  return if ref($cond) ne 'PPI::Structure::Condition';

  my @gparent_blocks = grep { ref($_) eq 'PPI::Structure::Block' 
			    } $gparent->elements();
  return if @gparent_blocks < 2 || $gparent_blocks[0] eq $parent;

  my @gparent_cond = grep { ref($_) eq 'PPI::Structure::Condition' 
			  } $gparent->elements();

  my $line = $gparent_cond[0]->line_number;
  $element->{__IF_LINE__} = "$file:$line";
  $element->{__CONDITIONS__} = [];
  my $ws = '';
  my $style = _display_style();
  for (my $i=0; $i<@gparent_blocks; $i++) {
    if ($i < @gparent_cond) {
      push @{$element->{__CONDITIONS__}}, 
	__new_token("${ws}ELSEIF "),
	$gparent_cond[$i]->clone();
    } else {
      push @{$element->{__CONDITIONS__}}, __new_token("${ws}ELSE");
    }
    if ($gparent_blocks[$i] eq $parent) {
      return;
    }
    $ws ||= $style == DISPLAY_TERSE ? "\n\t\t\t" : " ";
  }
}

sub __new_token {
  my ($text) = @_;
  my $element = bless { content => $text }, 'PPI::Token';
  return $element->clone();
}

our $NULL_DOC = '';
our $NULL_STATEMENT;
sub __new_null_statement {
  unless ($NULL_STATEMENT) {
    $NULL_DOC = PPI::Document->new(\' ');
    $NULL_STATEMENT = ($NULL_DOC->elements)[0];
  }
  return $NULL_STATEMENT->clone();
}

1;

__END__

=head1 NAME

Devel::DumpTrace::PPI - PPI-based version of Devel::DumpTrace

=head1 VERSION

0.09

=head1 SYNOPSIS

    perl -d:DumpTrace::PPI demo.pl
    >>>>> demo.pl:3:[__top__]:        $a:1 = 1;
    >>>>> demo.pl:4:[__top__]:        $b:3 = 3;
    >>>>> demo.pl:5:[__top__]:        $c:23 = 2 * $a:1 + 7 * $b:3;
    >>>>> demo.pl:6:[__top__]:        @d:(1,3,26) = ($a:1, $b:3, $c:23 + $b:3);

    perl -d:DumpTrace::PPI=verbose demo.pl
    >>   demo.pl:3:[__top__]:
    >>>              $a = 1;
    >>>>>            1 = 1;
    ------------------------------------------
    >>   demo.pl:4:[__top__]:
    >>>              $b = 3;
    >>>>>            3 = 3;
    ------------------------------------------
    >>   demo.pl:5:[__top__]:
    >>>              $c = 2 * $a + 7 * $b;
    >>>>             $c = 2 * 1 + 7 * 3;
    >>>>>            23 = 2 * 1 + 7 * 3;
    ------------------------------------------
    >>   demo.pl:6:[__top__]:
    >>>              @d = ($a, $b, $c + $b);
    >>>>             @d = (1, 3, 23 + 3);
    >>>>>            (1,3,26) = (1, 3, 23 + 3);
    ------------------------------------------

=head1 DESCRIPTION

C<Devel::DumpTrace::PPI> is a near drop-in replacement
to L<Devel::DumpTrace|Devel::DumpTrace> that uses the L<PPI module|PPI>
for parsing the source code.
PPI overcomes some of the limitations of the original C<Devel::DumpTrace>
parser, including

=over 4

=item * handling statements with chained assignments of complex assignment
expressions

  $ perl -d:DumpTrace=verbose -e '$a=$b[$c=2]="foo"'
  >>  -e:1:[__top__]:
  >>>              $a=$b[$c=2]="foo"
  >>>>             $a=()[undef=2]="foo"
  >>>>>            'foo'=()[undef=2]="foo"
  -------------------------------------------

  $ perl -d:DumpTrace::PPI=verbose -e '$a=$b[$c=2]="foo"'
  >>   -e:1:[__top__]:
  >>>              $a=$b[$c=2]="foo"
  >>>>>            'foo'=(undef,undef,'foo')[$c=2]="foo"
  ------------------------------------------

=item * multi-line statements

  $ cat multiline.pl
  $b = 4;
  @a = (1 + 2,
        3 + $b);

    $ perl -d:DumpTrace=verbose multiline.pl
    >>  multiline.pl:1:[__top__]:
    >>>              $b = 4;
    >>>>>            4 = 4;
    -------------------------------------------
    >>  multiline.pl:2:[__top__]:
    >>>              @a = (1 + 2,
    >>>>>            (3,7) = (1 + 2,
    -------------------------------------------

    $ perl -d:DumpTrace::PPI=verbose multiline.pl
    >>   multiline.pl:1:[__top__]:
    >>>              $b = 4;
    >>>>>            4 = 4;
    ------------------------------------------
    >>   multiline.pl:2:[__top__]:
    >>>              @a = (1 + 2,
                           3 + $b);
    >>>>             @a = (1 + 2,
                           3 + 4);
    >>>>>            (3,7) = (1 + 2,
                           3 + 4);
    ------------------------------------------

=item * string literals with variable names

    $ perl -d:DumpTrace=verbose -e '$email = q/mob@cpan.org/'
    >>  -e:1:[__top__]:
    >>>              $email = q/mob@cpan.org/
    >>>>             $email = q/mob().org/
    >>>>>            "mob\@cpan.org" = q/mob().org/
    -------------------------------------------

    $ perl -d:DumpTrace::PPI=verbose -e '$email = q/mob@cpan.org/'
    >>   -e:1:[__top__]:
    >>>              $email = q/mob@cpan.org/
    >>>>>            "mob\@cpan.org" = q/mob@cpan.org/
    ------------------------------------------

=item * Better recognition of Perl's magic variables

    $ perl -d:DumpTrace=verbose -e '$"="\t";'  -e 'print join $", 3, 4, 5'
    >>  -e:1:[__top__]:
    >>>              $"="\t";
    >>>>>            $"="\t";
    -------------------------------------------
    >>  -e:2:[__top__]:
    >>>              print join $", 3, 4, 5
    -------------------------------------------
    3       4       5

    $ perl -d:DumpTrace::PPI=verbose -e '$"="\t";' -e 'print join $", 3, 4, 5'
    >>   -e:1:[__top__]:
    >>>              $"="\t";
    >>>>>            "\t"="\t";
    ------------------------------------------
    >>   -e:2:[__top__]:
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

    >>    -e:1:[__top__]:
    >>>              $_=pop;
    >>>>             $_=pop ('hello');
    >>>>>            'hello'=pop ('hello');
    -------------------------------------------
    >>    -e:2:[__top__]:
    >>>              print m/hello/ && sin
    >>>>             print 'hello'=~m/hello/ && sin $_
    0>>>>>           print 'hello'=~m/hello/ && sin 'hello'
    -------------------------------------------

=back

The PPI-based parser has more overhead than the simpler parser from
L<Devel::DumpTrace|Devel::DumpTrace> (benchmarks forthcoming),
so you may not want to always favor
C<Devel::DumpTrace::PPI> over C<Devel::DumpTrace>.
Plus it requires L<PPI|PPI> to be installed. You can force 
the basic (non-PPI) parser to be used by either invoking your
program with the C<-d:DumpTrace::noPPI> switch, or by setting
the environment variable C<DUMPTRACE_NOPPI> to a true value.

See L<Devel::DumpTrace|Devel::DumpTrace> for far more information
about what this module is supposed to do, including the variables
and configuration settings.

=head1 SPECIAL HANDLING FOR FLOW CONTROL STRUCTURES

Inside a Perl debugger, there are many expressions evaluated
inside Perl flow control structures that "cannot hold a breakpoint"
(to use the language of L<perldebguts|perldebguts>). As a result,
these expressions never appear in a normal trace ouptut (using
C<-d:Trace>, for example).

For example, a trace for a line containing a C-style C<for> loop 
typically appears only once, during the first iteration of the loop:

    $ perl -d:Trace -e 'for ($i=0; $i<3; $i++) {' -e '$j = $i ** 2;' -e '}'
    >> -e:3: }
    >> -e:1: for ($i=0; $i<3; $i++) {
    >> -e:2: $j = $i ** 2;
    >> -e:2: $j = $i ** 2;
    >> -e:2: $j = $i ** 2;

Perl is supposed to be evaluating the expressions C<$i++> and C<< $i<3 >>
at each iteration, but those steps are optimized out of the trace output.

Or for another example, a trace through a complex C<if-elsif-else>
structure only produces the condition expression for the initial
C<if> statement:

    $ perl -d:Trace -e '$a=3;
    > if ($a==1) {
    >   $b=$a;
    > } elsif ($a==2) {
    >   $b=0;
    > } else {
    >   $b=9;
    > }'
    >> -e:1: $a=3;
    >> -e:2: if ($a==1) {
    >> -e:7:   $b=9;

To get to the assignment C<$b=9>, Perl needed to have evaluated
the expression C<$a==2>, but this step did not make it to the
trace output.

There's a lot of value in seeing these expressions, however,
so C<Devel::DumpTrace::PPI> takes steps to attach these
expressions to the existing source code and to display and
evaluate these expressions when they would have been evaluated
in the Perl program.

=head2 Special handling for C-style for loops

A C-style for loop has the structure

    for ( INITIALIZER ; CONDITION ; UPDATE ) BLOCK

In debugging a program with such a control structure, it is helpful
to observe how the C<CONDITION> and C<UPDATE> expressions are
evaluated at each iteration of the loop. At times the first statement
of a C<BLOCK> inside a for loop will be decorated with the relevant
expressions from the C<for> loop:

    $ cat ./simple-for.pl
    for ($i=0; $i<3; $i++) {
      $y += $i;
    }

    $ perl -d:DumpTrace::PPI ./simple-for.pl
    >>>   ./simple-for.pl:3:[__top__]:
    >>>>> ./simple-for.pl:1:[__top__]:      for ($i:0=0; $i:0<3; $i:0++) {
    >>>>> ./simple-for.pl:2:[__top__]:      $y:0 += $i:0;
    >>>>> ./simple-for.pl:2:[__top__]:      FOR-UPDATE: {$i:1++ } FOR-COND: {$i:1<3; }
                                            $y:1 += $i:1;
    >>>>> ./simple-for.pl:2:[__top__]:      FOR-UPDATE: {$i:2++ } FOR-COND: {$i:2<3; }
                                            $y:3 += $i:2;

    $ perl -d:DumpTrace::PPI=verbose ./simple-for.pl
    >>    ./simple-for.pl:3:[__top__]:
    >>>
    -------------------------------------------
    >>    ./simple-for.pl:1:[__top__]:
    >>>              for ($i=0; $i<3; $i++) {
    >>>>             for ($i=0; 0<3; 0++) {
    >>>>>            for (0=0; 0<3; 0++) {
    -------------------------------------------
    >>    ./simple-for.pl:2:[__top__]:
    >>>              $y += $i;
    >>>>             $y += 0;
    >>>>>            0 += 0;
    -------------------------------------------
    >>    ./simple-for.pl:2:[__top__]:
    >>>              FOR-UPDATE: {$i++ } FOR-COND: {$i<3; } $y += $i;
    >>>>             FOR-UPDATE: {1++ } FOR-COND: {1<3; } $y += 1;
    >>>>>            FOR-UPDATE: {1++ } FOR-COND: {1<3; } 1 += 1;
    -------------------------------------------
    >>    ./simple-for.pl:2:[__top__]:
    >>>              FOR-UPDATE: {$i++ } FOR-COND: {$i<3; } $y += $i;
    >>>>             FOR-UPDATE: {2++ } FOR-COND: {2<3; } $y += 2;
    >>>>>            FOR-UPDATE: {2++ } FOR-COND: {2<3; } 3 += 2;
    -------------------------------------------

The first time the loop's block code is executed, there is no need
to evaluate the conditional or the update expression, because
they were just evaluated in the previous line. But the second and third
time through the loop, the original source code is decorated with
C<FOR-UPDATE: {> I<expression> C<}> and C<FOR-COND: {> I<expression>
C<}>, showing what code was executed when the previous iteration 
finished, and what expression was evaluated to determine whether to
continue with the C<for> loop, respectively. 

Unfortunately, this example still does not show the expressions
that were evaluated at the end of the third loop, when the
update and condition expressions are evaluated another time.
In the last iteration, the condition expression evaluates to false
and the program breaks out of the loop.

=head2 Special handle for while/until loops

As with a C<for> loop, the conditional expression of a 
C<while> or C<until> loop is only included in trace output
on the initial entrance to the loop. C<Devel::DumpTrace::PPI>
decorates the first statement of the block inside the C<while/until>
loop to show how the conditional expression is evaluated at the
beginning of every iteration of the loop:

    $ cat ./simple-while.pl
    my ($i, $j, $l) = (0, 9, 0);
    while ($i++ < 6) {
      my $k = $i * $j--;
      next if $k % 5 == 1;
      $l = $l + $k;
    }

    $ perl -d:DumpTrace::PPI ./simple-while.pl   
    >>>   /tmp/while3.pl:1:[__top__]:
    >>>   /tmp/while3.pl:2:[__top__]:       while ($i:0++ < 6) {
    >>>>> /tmp/while3.pl:3:[__top__]:       my $k:9 = $i:1 * $j:9--;
    >>>   /tmp/while3.pl:4:[__top__]:       next if $k:9 % 5 == 1;
    >>>>> /tmp/while3.pl:5:[__top__]:       $l:9 = $l:0 + $k:9;
    >>>>> /tmp/while3.pl:3:[__top__]:       WHILE: ($i:2++ < 6)
                                            my $k:16 = $i:2 * $j:8--;
    >>>   /tmp/while3.pl:4:[__top__]:       next if $k:16 % 5 == 1;
    >>>>> /tmp/while3.pl:3:[__top__]:       WHILE: ($i:3++ < 6)
                                            my $k:21 = $i:3 * $j:7--;
    >>>   /tmp/while3.pl:4:[__top__]:       next if $k:21 % 5 == 1;
    >>>>> /tmp/while3.pl:3:[__top__]:       WHILE: ($i:4++ < 6)
                                            my $k:24 = $i:4 * $j:6--;
    >>>   /tmp/while3.pl:4:[__top__]:       next if $k:24 % 5 == 1;
    >>>>> /tmp/while3.pl:5:[__top__]:       $l:33 = $l:9 + $k:24;
    >>>>> /tmp/while3.pl:3:[__top__]:       WHILE: ($i:5++ < 6)
                                            my $k:25 = $i:5 * $j:5--;
    >>>   /tmp/while3.pl:4:[__top__]:       next if $k:25 % 5 == 1;
    >>>>> /tmp/while3.pl:5:[__top__]:       $l:58 = $l:33 + $k:25;
    >>>>> /tmp/while3.pl:3:[__top__]:       WHILE: ($i:6++ < 6)
                                            my $k:24 = $i:6 * $j:4--;
    >>>   /tmp/while3.pl:4:[__top__]:       next if $k:24 % 5 == 1;
    >>>>> /tmp/while3.pl:5:[__top__]:       $l:82 = $l:58 + $k:24;

In this example, a C<WHILE: {> I<expression> C<}> decorator
(capitalized to indicate that it is not a part of the actual source
code) shows how the conditional statement was evaluated prior to
each iteration of the loop (the output is a little misleading because
the conditional expression contains a C<++> postfix operator,
but this module does not evaluate the expression until after the
real conditional expression has actually been evaluated).

Again, the output does not show the conditional expression being
evaluated for the final time, right before the program breaks out
of the while loop control structure.

=head2 Complex if - elsif - ... - else blocks

Although a long sequence of expressions might need to be
evaluated to determine program flow through a complex 
C<if> - C<elsif> - ... - C<else> statement, the normal trace output
will always only show the initial condition (that is, the
condition associated with the C<if> keyword). C<Devel::DumpTrace::PPI>
will decorate the first statement in blocks after the C<elsif>
or C<else> keywords to show all of the expressions that 
had to be evaluated to get to a particular point of execution, and
how (subject to side-effects of the conditional expressions) those
expressions were evaluated:

    $ cat ./if-elsif-else.pl
    for ($a=-1; $a<=3; $a++) {
      if ($a == 1) {
        $b = 1;
      } elsif ($a == 2) {
        $b = 4;
      } elsif ($a == 3) {
        $b = 9;
      } elsif ($a < 0) {
        $b = 5;
        $b++;
      } else {
        $b = 20;
      }
    }

    $ perl -d:DumpTrace::PPI ./if-elsif-else.pl
    >>>   /tmp/if4.pl:14:[__top__]:
    >>>>> /tmp/if4.pl:1:[__top__]:  for ($a:-1=-1; $a:-1<=3; $a:-1++) {
    >>>   /tmp/if4.pl:2:[__top__]:  if ($a:-1 == 1) {
    >>>>> /tmp/if4.pl:9:[__top__]:  ELSEIF ($a:-1 == 1)
                                            ELSEIF ($a:-1 == 2)
                                            ELSEIF ($a:-1 == 3)
                                            ELSEIF ($a:-1 < 0)
                                            $b:5 = 5;
    >>>   /tmp/if4.pl:10:[__top__]: $b:5++;
    >>>   /tmp/if4.pl:2:[__top__]:  FOR-UPDATE: {$a:0++ } FOR-COND: {$a:0<=3; }
                                            if ($a:0 == 1) {
    >>>>> /tmp/if4.pl:12:[__top__]: ELSEIF ($a:0 == 1)
                                            ELSEIF ($a:0 == 2)
                                            ELSEIF ($a:0 == 3)
                                            ELSEIF ($a:0 < 0)
                                            ELSE
                                            $b:20 = 20;
    >>>   /tmp/if4.pl:2:[__top__]:  FOR-UPDATE: {$a:1++ } FOR-COND: {$a:1<=3; }
                                            if ($a:1 == 1) {
    >>>>> /tmp/if4.pl:3:[__top__]:  $b:1 = 1;
    >>>   /tmp/if4.pl:2:[__top__]:  FOR-UPDATE: {$a:2++ } FOR-COND: {$a:2<=3; }
                                            if ($a:2 == 1) {
    >>>>> /tmp/if4.pl:5:[__top__]:  ELSEIF ($a:2 == 1)
                                            ELSEIF ($a:2 == 2)
                                            $b:4 = 4;
    >>>   /tmp/if4.pl:2:[__top__]:  FOR-UPDATE: {$a:3++ } FOR-COND: {$a:3<=3; }
                                            if ($a:3 == 1) {
    >>>>> /tmp/if4.pl:7:[__top__]:  ELSEIF ($a:3 == 1)
                                            ELSEIF ($a:3 == 2)
                                            ELSEIF ($a:3 == 3)
                                            $b:9 = 9;

In this example, the C<ELSEIF> (I<expression>) and C<ELSE> decorators
indicate what expressions must have been evaluated to reach
the particular block of the statement that is to be executed.

=head2 Some TODOs

Other flow control structures in Perl could benefit from 
similar treatment. These will be addressed in future versions
of this distribution.

=over 4

=item C<do-while> and C<do-until> loops

=item C<given-when> blocks

=item C<for[each]> [variable] LIST loops

=item C<map BLOCK LIST> and C<grep BLOCK LIST> blocks

=item postfix C<EXPR for LIST>, C<EXPR while/until CONDITION> statements

Postfix expressions are typically executed as a single operation
(with no place to hold a breakpoint). This item might need some L<B-fu|B>.

=back

=head1 SUBROUTINES/METHODS

None to worry about.

=head1 EXPORT

Nothing is or can be exported from this module.

=head1 DIAGNOSTICS

None

=head1 CONFIGURATION AND ENVIRONMENT

Like L<Devel::DumpTrace|Devel::DumpTrace>, this module also reads
the C<DUMPTRACE_FH> and C<DUMPTRACE_LEVEL> environment variables.
See C<Devel::DumpTrace> for details about how to use these variables.

=head1 DEPENDENCIES

L<PPI|PPI> for understanding the structure of your Perl script.

L<PadWalker|PadWalker> for arbitrary access to lexical variables.

L<Scalar::Util|Scalar::Util> for the reference identification
convenience methods.

L<Text::Shorten|Text::Shorten> (bundled with this distribution)
for abbreviating long output, when desired.

=head1 INCOMPATIBILITIES

None known.

=head1 BUGS AND LIMITATIONS

Parses C<-e> code differently than code from a source file. This
is to be remedied in a future release.

See L<Devel::DumpTrace/"SUPPORT"> for other support information.
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
