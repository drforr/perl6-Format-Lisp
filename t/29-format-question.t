use v6;

use Test;
use lib 't/lib';
use Utils;
use Format::Lisp;

my $fl = Format::Lisp.new;

# (def-format-test format.?.1
#   "~?" ("" nil) "")
# 
is $fl.format( Q{~?}, '', Nil ), Q{}, 'format.?.1';

#`(
# (def-format-test format.?.2
#   "~?" ("~A" '(1)) "1")
# 
is $fl.format( Q{~?}, '~A', [ 1 ] ), Q{1}, 'format.?.2';
)

# (def-format-test format.?.3
#   "~?" ("" '(1)) "")
# 
is $fl.format( Q{~?}, '', [ 1 ] ), Q{}, 'format.?.3';

#`(
# (def-format-test format.?.4
#   "~? ~A" ("" '(1) 2) " 2")
# 
is $fl.format( Q{~? ~A}, '', [ 1 ], 2 ), Q{ 2}, 'format.?.4';
)

#`(
# (def-format-test format.?.5
#   "a~?z" ("b~?y" '("c~?x" ("~A" (1)))) "abc1xyz")
# 
is $fl.format(
	Q{a~?z},
	[ "b~?x", [ "c~?x", [ "~A", [ 1 ] ] ] ]
), Q{abc1xyz}, 'format.?.5';
)

# ;;; Tests of ~@?
# 
# (def-format-test format.@?.1
#   "~@?" ("") "")
# 
is $fl.format( Q{~@?}, '' ), Q{}, 'format.@?.1';

#`(
# (def-format-test format.@?.2
#   "~@?" ("~A" 1) "1")
# 
is $fl.format( Q{~@?}, "~A", 1 ), Q{1}, 'format.@?.2';
)

#`(
# (def-format-test format.@?.3
#   "~@? ~A" ("<~A>" 1 2) "<1> 2")
# 
is $fl.format( Q{~@? ~A}, "<~A>", 1, 2 ), Q{<1> 2}, 'format.@?.3';
)

#`(
# (def-format-test format.@?.4
#   "a~@?z" ("b~@?y" "c~@?x" "~A" 1) "abc1xyz")
# 
is $fl.format(
	Q{a~@?z},
	"b~@?y", "c~@?x", "~A", 1
), Q{abc1xyz}, 'format.@?.4';
)

#`(
# (def-format-test format.@?.5
#   "~{~A~@?~A~}" ('(1 "~4*" 2 3 4 5 6)) "16")
# 
is $fl.format(
	Q{~{~A~@?~A~}},
	1, "~4*", 2, 3, 4, 5, 6
), Q{16}, 'format.@?.5';
)

done-testing;

# vim: ft=perl6
