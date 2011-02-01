use Devel::DumpTrace;
use Test::More tests => 20;
use strict;
use warnings;

# test settings for $Devel::DumpTrace::TRACE

for my $lev (qw(1 2 3 4 5 6 7 11 12 13 314159 102 106 999)) {
  $Devel::DumpTrace::TRACE = $lev;
  ok($Devel::DumpTrace::TRACE == $lev);
}

$Devel::DumpTrace::TRACE = 'default';
ok($Devel::DumpTrace::TRACE == 3);

$Devel::DumpTrace::TRACE = 'normal';
ok($Devel::DumpTrace::TRACE == 3);

$Devel::DumpTrace::TRACE = 'quiet';
ok($Devel::DumpTrace::TRACE == 1);

$Devel::DumpTrace::TRACE = 'verbose';
ok($Devel::DumpTrace::TRACE == 5);

$Devel::DumpTrace::TRACE = 'verbose,package';
ok($Devel::DumpTrace::TRACE == 105);

$Devel::DumpTrace::TRACE = 'quiet,package';
ok($Devel::DumpTrace::TRACE == 101);
