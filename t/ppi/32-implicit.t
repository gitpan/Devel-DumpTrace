package ImplicitTest;
use Test::More tests => 8;
use Devel::DumpTrace::PPI;
use PPI;
use strict;
use warnings;

Devel::DumpTrace::import_all();
*preval = \&Devel::DumpTrace::preval;

sub to_PPI_Statement {
  my $code = shift;
  $::doc = new PPI::Document(\$code);  # must keep document in scope
  my $s = $::doc->find('PPI::Statement');
  return $s->[0];
}

# implicit $_ =~ qr/regexp/
# implicit $_ in built in functions
# implicit @_ or @ARGV in shift, pop

$_ = "FOOasdfBAR";
my $doc = new PPI::Document(\'m{asdf} && print "Contains asdf\n"');
my $s = $doc->find('PPI::Statement');
my @z = preval($s->[0], 1, __PACKAGE__);
ok("@z" =~ /\$_:.*$_.*=~\s*m\{asdf\}/,
   "implicit \$_=~ inserted before regexp");

$s = to_PPI_Statement('s/hello/hey/i && print "$_ world\n"');
@z = preval($s, 1, __PACKAGE__);
ok("@z" =~ m!\$_:.*$_.*=~\s*s/hello/hey/i!,
   "implicit \$_=~ inserted before substitution");


@z = preval(
	    to_PPI_Statement('my $z = log;'),
	    1, __PACKAGE__);
ok("@z" =~ /\$_/, 
   "inserted implicit \$_ for builtin function");

@z = preval(
	    to_PPI_Statement('my $z = ref'),
	    1, __PACKAGE__);
ok("@z" =~ /\$_/, 
   "inserted implicit \$_ for builtin function");

@z = preval(
	    to_PPI_Statement('my $z = shift'),
	    1, __PACKAGE__);
ok("@z" =~ /\@ARGV/,
   "inserted implicit \@ARGV to shift/pop call");


sub naked_pop_inside_sub_test {
  my @z = preval(
	    to_PPI_Statement('$b = pop'),
	    1, __PACKAGE__);
  ok("@z" =~ /\@_/,
     "inserted implicit \@_ after shift/pop call inside sub")
  or diag(@z);
}
&naked_pop_inside_sub_test();


@z = preval(
	    to_PPI_Statement('if (-f)'),
	    1, __PACKAGE__);
ok("@z" =~ /-f\s+\$_/, 
   "inserted implicit \$_ for file test");

@z = preval(
	    to_PPI_Statement('if (-t)'),
	    1, __PACKAGE__);
ok("@z" !~ /\$_/,
   "no implicit \$_ for -t file test");

