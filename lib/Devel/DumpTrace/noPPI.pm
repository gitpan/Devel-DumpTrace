### loads Devel::DumpTrace with an additional flag 
### to inhibit using Devel::DumpTrace::PPI

BEGIN {
  $Devel::DumpTrace::NO_PPI = 1;
}
use Devel::DumpTrace;

sub import {
  return Devel::DumpTrace::import(@_);
}

1;
