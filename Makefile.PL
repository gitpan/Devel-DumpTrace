use strict;
use warnings;
use ExtUtils::MakeMaker;

my $TESTS = "t/*.t t/ppi/*.t";
unless (eval 'use PPI;1') {
  print q[
This distribution comes with a PPI-based module.
Install the PPI distribution on your system
to make use of it.

];
  $TESTS = "t/*.t t/ppi/00-load.t";
}

# no index instructions for PAUSE
my $NO_INDEX = 
  { 'package' =>
    ['Text::Shorten', 'Devel::DumpTrace::CachedDisplayedArray',
     'Devel::DumpTrace::CachedDisplayedHash', 'Devel::DumpTrace::noPPI'] };

WriteMakefile(
    NAME                => 'Devel::DumpTrace',
    AUTHOR              => q{Marty O'Brien <mob@cpan.org>},
    VERSION_FROM        => 'lib/Devel/DumpTrace.pm',
    ABSTRACT_FROM       => 'lib/Devel/DumpTrace.pm',
    ($ExtUtils::MakeMaker::VERSION+0 >= 6.3002
      ? ('LICENSE'=> 'perl')
      : ()),
    ($ExtUtils::MakeMaker::VERSION+0 >= 6.46
      ? ('META_MERGE'   => { no_index => $NO_INDEX })
      : ()),
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
	'PadWalker' => 1.5,
        'Scalar::Util' => '1.14',     # 1.14 has improved reftype()
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Devel-DumpTrace-*' },
    test                => { TESTS => $TESTS },
);
