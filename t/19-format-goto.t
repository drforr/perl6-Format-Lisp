use v6;

use Test;
use lib 't/lib';
use Utils;
use Format::Lisp;

my $fl = Format::Lisp.new;

# ;;; ~*
# 
#`(
# (def-format-test format.*.1
#   "~A~*~A" (1 2 3) "13")
# 
is $fl.format( Q{~A~*~A}, 1, 2, 3 ), "13", 'format.*.1';
)

#`(
# (def-format-test format.*.2
#   "~A~0*~A" (1 2 3) "12" 1)
# 
is $fl.format( Q{~A~0*~A}, 1, 2, 3 ), "12", 'format.*.2';
)

#`(
# (def-format-test format.*.3
#   "~A~v*~A" (1 0 2) "12")
# 
is $fl.format( Q{~A~v*~A}, 1, 0, 2 ), "12", 'format.*.3';
)

#`(
# (def-format-test format.*.4
#   "~A~v*~A" (1 1 2 3) "13")
# 
is $fl.format( Q{~A~v*~A}, 1, 1, 2, 3 ), "13", 'format.*.4';
)

#`(
# (def-format-test format.*.5
#   "~A~v*~A" (1 nil 2 3) "13")
# 
is $fl.format( Q{~A~v*~A}, 1, Nil, 2, 3 ), "13", 'format.*.5';
)

#`(
# (def-format-test format.*.6
#   "~A~1{~A~*~A~}~A" (0 '(1 2 3) 4) "0134")
# 
is $fl.format( Q{~A~1{~A~*~A~}~A}, 0, [ 1, 2, 3 ], 4 ), "0134", 'format.*.6';
)

#`(
# (def-format-test format.*.7
#   "~A~1{~A~0*~A~}~A" (0 '(1 2 3) 4) "0124")
# 
is $fl.format( Q{~A~1{~A~0*~A~}~A}, 0, [ 1, 2, 3 ], 4 ), "0124", 'format.*.7';
)

#`(
# (def-format-test format.*.8
#   "~A~{~A~*~A~}~A" (0 '(1 2 3 4 5 6) 7) "013467")
# 
is $fl.format(
	Q{~A~1{~A~*~A~}~A},
	0, [ 1, 2, 3, 4, 5, 6 ], 7
), "013467", 'format.*.8';
)

#`(
# (def-format-test format.*.9
#   "~A~{~A~A~A~A~v*~^~A~A~A~A~}~A" (0 '(1 2 3 4 nil 6 7 8 9 #\A) 5)
#   "01234789A5")
# 
is $fl.format(
	Q{~A~{~A~A~A~A~v*~^~A~A~A~A~}~A},
	0, [ 1, 2, 3, 4, Nil, 6, 7, 8, 9, 'A' ], 5
), "01234789A5", 'format.*.9';
)

# ;;; ~:*
# 
#`(
# (def-format-test format.\:*.1
#   "~A~:*~A" (1 2 3) "11" 2)
# 
is $fl.format( Q{~A~:*~A}, 1, 2, 3 ), "11", 'format.:*.1';
)

#`(
# (def-format-test format.\:*.2
#   "~A~A~:*~A" (1 2 3) "122" 1)
# 
is $fl.format( Q{~A~A~:*~A}, 1, 2, 3 ), "122", 'format.:*.2';
)

#`(
# (def-format-test format.\:*.3
#   "~A~A~0:*~A" (1 2 3) "123")
# 
is $fl.format( Q{~A~A~0:*~A}, 1, 2, 3 ), "123", 'format.:*.3';
)

#`(
# (def-format-test format.\:*.4
#   "~A~A~2:*~A" (1 2 3) "121" 2)
# 
is $fl.format( Q{~A~A~2:*~A}, 1, 2, 3 ), "121", 'format.:*.4';
)

#`(
# (def-format-test format.\:*.5
#   "~A~A~v:*~A" (1 2 0 3) "123")
# 
is $fl.format( Q{~A~A~v:*~A}, 1, 2, 0, 3 ), "123", 'format.:*.5';
)

#`(
# (def-format-test format.\:*.6
#   "~A~A~v:*~A" (6 7 2 3) "677" 2)
# 
is $fl.format( Q{~A~A~v:*~A}, 6, 7, 2, 3 ), "677", 'format.:*.6';
)

#`(
# (def-format-test format.\:*.7
#   "~A~A~v:*~A" (6 7 nil 3) "67NIL" 1)
# 
is $fl.format( Q{~A~A~v:*~A}, 6, 7, Nil, 3 ), "67NIL", 'format.:*.7';
)

#`(
# (def-format-test format.\:*.8
#   "~A~1{~A~:*~A~}~A" (0 '(1 2 3) 4) "0114")
# 
is $fl.format( Q{~A~1{~A~:*~A~}~A}, 0, [ 1, 2, 3 ], 4 ), "0114", 'format.:*.8';
)

#`(
# (def-format-test format.\:*.9
#   "~A~1{~A~A~A~:*~A~}~A" (0 '(1 2 3 4) 5) "012335")
# 
is $fl.format(
	Q{~A~1{~A~A~A~:*~A~}~A},
	0, [ 1, 2, 3, 4 ], 5
), "012335", 'format.:*.9';
)

#`(
# (def-format-test format.\:*.10
#   "~A~1{~A~A~A~2:*~A~A~}~A" (0 '(1 2 3 4) 5) "0123235")
# 
is $fl.format(
	Q{~A~1{~A~A~A~2:*~A~A~}~A},
	0, [ 1, 2, 3, 4 ], 5
), "0123235", 'format.:*.10';
)

#`(
# (def-format-test format.\:*.11
#   "~A~{~A~A~A~3:*~A~A~A~A~}~A" (0 '(1 2 3 4) 5) "012312345")
# 
is $fl.format(
	Q{~A~{~A~A~A~3:*~A~A~A~A~}~A},
	0, [ 1, 2, 3, 4 ], 5
), "012312345", 'format.:*.11';
)

#`(
# (def-format-test format.\:*.12
#   "~A~{~A~A~A~A~4:*~^~A~A~A~A~}~A" (0 '(1 2 3 4) 5) "0123412345")
# 
is $fl.format(
	Q{~A~{~A~A~A~A~4:*~^~A~A~A~A~}~A},
	0, [ 1, 2, 3, 4 ], 5
), "0123412345", 'format.:*.12';
)

#`(
# (def-format-test format.\:*.13
#   "~A~{~A~A~A~A~v:*~^~A~}~A" (0 '(1 2 3 4 nil) 5) "01234NIL5")
# 
is $fl.format(
	Q{~A~{~A~A~A~A~v:*~^~A~}~A},
	0, [ 1, 2, 3, 4, Nil ], 5
), "01234NIL5", 'format.:*.13';
)

# ;;; ~@*
# 
#`(
# (def-format-test format.@*.1
#   "~A~A~@*~A~A" (1 2 3 4) "1212" 2)
# 
is $fl.format( Q{~A~A~@*~A~A}, 1, 2, 3, 4 ), Q{1212}, 'format.@*.1';
)

#`(
# (def-format-test format.@*.2
#   "~A~A~1@*~A~A" (1 2 3 4) "1223" 1)
# 
is $fl.format( Q{~A~A~1@*~A~A}, 1, 2, 3, 4 ), Q{1223}, 'format.@*.2';
)

#`(
# (def-format-test format.@*.3
#   "~A~A~2@*~A~A" (1 2 3 4) "1234")
# 
is $fl.format( Q{~A~A~2@*~A~A}, 1, 2, 3, 4 ), Q{1234}, 'format.@*.3';
)

#`(
# (def-format-test format.@*.4
#   "~A~A~3@*~A~A" (1 2 3 4 5) "1245")
# 
is $fl.format( Q{~A~A~3@*~A~A}, 1, 2, 3, 4, 5 ), Q{1245}, 'format.@*.4';
)

#`(
# (def-format-test format.@*.5
#   "~A~A~v@*~A~A" (1 2 nil 3 4) "1212" 3)
# 
is $fl.format( Q{~A~A~v@*~A~A}, 1, 2, Nil, 3, 4 ), Q{1212}, 'format.@*.5';
)

#`(
# (def-format-test format.@*.6
#   "~A~A~v@*~A~A" (1 2 1 3 4) "1221" 2)
# 
is $fl.format( Q{~A~A~v@*~A~A}, 1, 2, 1, 3, 4 ), Q{1221}, 'format.@*.6';
)

#`(
# (def-format-test format.@*.7
#   "~A~A~v@*~A~A" (6 7 2 3 4) "6723" 1)
# 
is $fl.format( Q{~A~A~v@*~A~A}, 6, 7, 2, 3, 4 ), Q{6723}, 'format.@*.7';
)

#`(
# (def-format-test format.@*.8
#   "~A~{~A~A~@*~A~A~}~A" (0 '(1 2) 9) "012129")
# 
is $fl.format(
	Q{~A{~A~A~@*~A~A~}~A},
	0, [ 1, 2 ], 9
), Q{012129}, 'format.@*.8';
)

#`(
# (def-format-test format.@*.9
#   "~A~{~A~A~0@*~A~A~}~A" (0 '(1 2) 9) "012129")
# 
is $fl.format(
	Q{~A{~A~A~0@*~A~A~}~A},
	0, [ 1, 2 ], 9
), Q{012129}, 'format.@*.9';
)

#`(
# (def-format-test format.@*.10
#   "~A~1{~A~A~v@*~A~A~}~A" (0 '(1 2 nil) 9) "012129")
# 
is $fl.format(
	Q{~A~1{~A~A~v@*~A~A~}~A},
	0, [ 1, 2, Nil ], 9
), Q{012129}, 'format.@*.10';
)

#`(
# (def-format-test format.@*.11
#   "~A~{~A~A~1@*~A~}~A" (0 '(1 2) 9) "01229")
# 
is $fl.format(
	Q{~A1{~A~A~1@*~A~}~A},
	0, [ 1, 2 ], 9
), Q{01229}, 'format.@*.11';
)

done-testing;
