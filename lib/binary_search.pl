# Devel::DumpTrace use case:
# broken binary search function.

# so we have written this binary search function:

sub binary_search {
  my ($target, @list) = @_;
  my ($lo,$hi,$mid,$w) = (0,$#list);
  while ($hi - $lo > 1) {
    $mid = int(($lo+$hi)/2);
    $w = $list[$mid];
    if ($target lt $w) {
      $hi = $mid - 1;
    } else {
      $lo = $mid + 1
    }
  }
  if ($list[$lo] ge $target) {
    return $lo;
  } else {
    return $hi;
  }
}

$Devel::DumpTrace::LEVEL = 0;

# at first glance it looks correct but it fails on some test cases

@w = sort qw(the quick brown fox jumps over the lazy dog);
for $w (@w) {
  $ww = binary_search($w, @w);
  if ($w[$ww] eq $w) { 
    print "$ww $w OK\n";
  } else { 
    print "$ww $w FAIL\n";
  }
}

# for $w := 'lazy', the return value is 5 but it should be 4.
# Why did this fail?
# Let's debug it with Devel::DumpTrace

BEGIN { $Devel::DumpTrace::TRACE = 0 };


$Devel::DumpTrace::TRACE = 'verbose';
  
$ww = binary_search("lazy", @w);
print "\$ww is $ww, should be 1.\n";

$Devel::DumpTrade::TRACE = 0;





