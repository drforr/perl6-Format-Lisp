use v6;

use Test;
use Format::Lisp;

plan 4;

my $fl = Format::Lisp.new;
my $*CONSISTENCY-CHECK = True;
my $*FALL-THROUGH = True;
my $parsed;

subtest {
	my @options =
		Q{~#,#@A},
		Q{~#,#A},
		Q{~#@A},
		Q{~#@a},
		Q{~#A},
		Q{~#a},
		Q{~-100000000000000000000a},
		Q{~-100A},
		Q{~10,,,v@A},
		Q{~10,,,v@a},
		Q{~10,,,vA},
		Q{~10,,,va},
		Q{~3,,+2A},
		Q{~3,,-1A},
		Q{~3,,0A},
		Q{~3,,v@A},
		Q{~3,,vA},
		Q{~3,1a},
		Q{~3,3@a},
		Q{~4,#@A},
		Q{~4,#A},
		Q{~4,,,'X@a},
		Q{~4,,,'XA},
		Q{~4,,,@A},
		Q{~4,,,a},
		Q{~4,,va},
		Q{~4,3a},
		Q{~4,4@a},
		Q{~5,#@A},
		Q{~5,#a},
		Q{~5,3@a},
		Q{~5,3A},
		Q{~5,v@A},
		Q{~5,vA},
		Q{~7,3@a},
		Q{~7,3A},
		Q{~:A},
		Q{~:a},
		Q{~? ~A},
		Q{~@? ~A},
		Q{~@A},
		Q{~@[X~]Y~A},
		Q{~@a},
		Q{~@{~2,#^~A~}X~A},
		Q{~A},
		Q{~AY~?X~A},
		Q{~AY~@?X~A},
		Q{~A~*~A},
		Q{~A~0*~A},
		Q{~A~1{~A~*~A~}~A},
		Q{~A~1{~A~0*~A~}~A},
		Q{~A~1{~A~:*~A~}~A},
		Q{~A~1{~A~A~A~2:*~A~A~}~A},
		Q{~A~1{~A~A~A~:*~A~}~A},
		Q{~A~1{~A~A~v@*~A~A~}~A},
		Q{~A~:*~A},
		Q{~A~?X~A},
		Q{~A~@?X~A},
		Q{~A~A~0:*~A},
		Q{~A~A~1@*~A~A},
		Q{~A~A~2:*~A},
		Q{~A~A~2@*~A~A},
		Q{~A~A~3@*~A~A},
		Q{~A~A~:*~A},
		Q{~A~A~@*~A~A},
		Q{~A~A~v:*~A},
		Q{~A~A~v@*~A~A},
		Q{~A~v*~A},
		Q{~A~{~A~*~A~}~A},
		Q{~A~{~A~A~0@*~A~A~}~A},
		Q{~A~{~A~A~1@*~A~}~A},
		Q{~A~{~A~A~@*~A~A~}~A},
		Q{~A~{~A~A~A~3:*~A~A~A~A~}~A},
		Q{~A~{~A~A~A~A~4:*~^~A~A~A~A~}~A},
		Q{~A~{~A~A~A~A~v*~^~A~A~A~A~}~A},
		Q{~A~{~A~A~A~A~v:*~^~A~}~A},
		Q{~V:@A},
		Q{~V:@a},
		Q{~V:A},
		Q{~V:a},
		Q{~V@:A},
		Q{~V@:a},
		Q{~V@A},
		Q{~V@a},
		Q{~VA},
		Q{~Va},
		Q{~a},
		Q{~v,,2A},
		Q{~v:@A},
		Q{~v:@a},
		Q{~v:A},
		Q{~v:a},
		Q{~v@:A},
		Q{~v@:a},
		Q{~v@A},
		Q{~v@a},
		Q{~vA},
		Q{~va},
		Q{~{~2,#^~A~}~A},
		Q{~~~d:a},
		Q{~~~d@:A},
		Q{~~~d@a},
		Q{~~~da},
	;
	for @options -> $str {
		ok $fl._parse( $str ), $str;
	}
}

subtest {
	my @options =
		Q{~b},
		Q{~@b},
		Q{~~~db},
		Q{~~~d@b},
		Q{~v,vb},
		Q{~:b},
		Q{~,,v:b},
		Q{~,,V,V:b},
		Q{~,,v,v:@b},
		Q{~vb},
		Q{~,,v:b},
		Q{~:@b},
		Q{~#b},
		Q{~,,,#:b},
		Q{~+10b},
		Q{~-1b},
		Q{~vb},
		Q{~db},
		Q{~v,v,v,vb},
		Q{~B},
		Q{~@B},
		Q{~v,vB},
		Q{~:B},
		Q{~,,v:B},
		Q{~,,V,V@:B},
		Q{~@B},
		Q{~,,v,v:B},
		Q{~6,vB},
		Q{~,,'*,v:B},
		Q{~:B},
		Q{~@:B},
		Q{~#B},
		Q{~,,,#:B},
		Q{~,,,#@:B},
		Q{~+10@B},
		Q{~-1000000000000000000B},
		Q{~V,V,V,VB},
	;
	for @options -> $str {
		ok $fl._parse( $str ), $str;
	}
}

subtest {
	my @options =
		Q{~~~d,'~c~c},
		Q{~~,,'~c:~c},
		Q{~:c},
		Q{~@c},
		Q{'~c},
		Q{~~~d,'~c~c},
		Q{~~,,'~c:~c},
		Q{#\\~:c},
		Q{~c},
		Q{~@:c},
		Q{~~~d~c},
		Q{~~~d@~c},
		Q{~~~d,'~c~c},
		Q{~~,,'~c:~c},
		Q{~C},
		Q{~:C},
		Q{~@C},
		Q{~:@C},
		Q{~C},
		Q{~:C},
		Q{~@C},
		Q{~@:C},
	;
	for @options -> $str {
		ok $fl._parse( $str ), $str;
	}
}

subtest {
	my @options =
		Q{~@d},
		Q{~~~dd},
		Q{~d},
		Q{~~~dd},
		Q{~~~d@d},
		Q{~~~d,'~cd},
		Q{~v,vd},
		Q{~@d},
		Q{~v,v@d},
		Q{~:d},
		Q{~,,v:d},
		Q{~,,v:d},
		Q{~~,,'~c:d},
		Q{~,,v,v:d},
		Q{~@d},
		Q{~,,v,v:@d},
		Q{~,,v:d},
		Q{~,,'*,v:d},
		Q{~@d},
		Q{~@:d},
		Q{~#d},
		Q{~,,,#:d},
		Q{~,,,#:@d},
		Q{~+10d},
		Q{~+10@d},
		Q{~-1d},
		Q{~-1000000000000000000d},
		Q{~vd},
		Q{~dd},
		Q{~v,v,v,vd},
		Q{~,,,#@:D},
		Q{~v,v,v,vD},
		Q{~D},
		Q{~@D},
		Q{~v,vD},
		Q{~v,v@D},
		Q{~,,v,v:D},
		Q{~,,v,v:@D},
		Q{~vD},
		Q{~6,vD},
		Q{~#D},
		Q{~,,,#:D},
	;
	for @options -> $str {
		ok $fl._parse( $str ), $str;
	}
}

# ----------------------------------




#  "~0&"
#  "~&"
#  "X~&"
#  "X~%~&"
#  "~~~D&"
#  "~~~D&"
#  "X~~~D&"
#  "X~~~D&"
#  "~v&"
#  "X~v&"
#  "~V&"
#  "~V&"
#  "~#&"
#  "~#&"
#  "X~V%"
#  "X~#%"
#  "X~#%"
#
#                (if mincol (format nil "~~~d," mincol) "~,")
#                (if padchar (format nil "'~c," padchar) ",")
#                (if commachar (format nil "'~c," commachar) ",")
#
#  "~{~}"
#
#  "~0{~}"
#
#  "~{ ~}"
#
#  "~{X Y Z~}"
#
#  "~{~A~}"
#
#  "~{~{~A~}~}"
#
#  "~{~1{~A~}~}"
#
#        for s = (format nil "~v{~A~}" i '(1 2 3 4 5 6 7 8 9 0))
#
#  (let ((fn (formatter "~V{~A~}")))
#
#  "~#{~A~}"
#
#  "~0{~}"
#
#  "~1{~}"
#
#  (format nil "~{~}" (formatter "") nil)
#
#  (format nil "~1{~}" (formatter "") '(1 2 3 4))
#
#  (format nil "~{~}"
#
#  (format nil "~3{~}"
#
#  "~V{~}"
#
#  "~#{~}"
#
#  "~{FOO~:}"
#
#  "~{~A~:}"
#
#  "~{~A~:}"
#
#  "~{~A~:}"
#
#  "~0{FOO~:}"
#
#  "~V{FOO~:}"
#
#  "~1{FOO~:}"
#
#  "~2{FOO~:}"
#
#  "~2{FOO~}"
#
#  "~v{~a~}"
#
#  "~:{(~A ~A)~}"
#
#  "~:{~}"
#
#  "~:{~}"
#
#  "~:{~}"
#
#  (format nil "~:{~}"
#
#  "~0:{XYZ~}"
#
#  "~2:{XYZ~}"
#
#  "~2:{~A~}"
#
#  "~2:{~A~}"
#
#        (format nil "~v:{~A~}" i '((1) (2) (3 X) (4 Y Z) (5) (6))))
#
#  (let ((fn (formatter "~v:{~A~}")))
#
#  "~V:{X~}" (nil '((1) (2) (3) nil (5))) "XXXXX")
#
#  "~#:{~A~}"
#
#  "~:{~A~:}"
#
#        (format nil "~v:{~A~:}" i '((1 X) (2 Y) (3) (4 A B))))
#
#  (let ((fn (formatter "~v:{~A~:}")))
#
#  "~:{ABC~:}"
#
#  "~v:{ABC~:}"
#
#  "~@{~}"
#
#  "~@{ ~}"
#
#  "~@{X ~A Y Z~}"
#
#  "~@{~A~}"
#
#  "~@{~{~A~}~}"
#
#  "~@{~1{~A~}~}"
#
#  "~1@{FOO~}"
#
#  "~v@{~A~}"
#
#  "~#@{~A~}"
#
#        "~v@{~A~}"
#
#  (let ((fn (formatter "~v@{~A~}")))
#
#  "~@{X~:}"
#
#  "~@{~}"
#
# (formatter "X~AY")
#
#  "~v@{~}"
# (formatter "X")
#
#  "~:@{~A~}"
#
#  "~:@{(~A ~A)~}"
#
#  "~:@{~}"
#
#  "~:@{~}"
#
#  (formatter "(~A ~A)")
#
#  "~:@{~A~:}"
#
#  "~0:@{~A~:}"
#
#  "~#:@{A~:}"
#
#  "~v:@{~A~}"
#
#        (apply #'format nil "~V:@{~A~}" i (reverse x)))
#
#  (let ((fn (formatter "~V@:{~A~}")))
#
#  (signals-type-error x 'A (format nil "~{~A~}" x))
#
#  (signals-type-error x 1 (format nil "~{~A~}" x))
#
#  (signals-type-error x "foo" (format nil "~{~A~}" x))
#
#  (signals-type-error x #*01101 (format nil "~{~A~}" x))
#
#  (signals-error (format nil "~{~A~}" '(x y . z)) type-error)
#
#  (signals-error (format nil "~:{~A~}" '(x)) type-error)
#
#  (signals-type-error x 'x (format nil "~:{~A~}" x))
#
#  (signals-error (format nil "~:{~A~}" '((x) . y)) type-error)
#
#  (signals-error (format nil "~:{~A~}" '("X")) type-error)
#
#  (signals-error (format nil "~:{~A~}" '(#(X Y Z))) type-error)
#
#  (signals-type-error x 'x (format nil "~:@{~A~}" x))
#
#  (signals-type-error x 0 (format nil "~:@{~A~}" x))
#
#  (signals-type-error x #*01101 (format nil "~:@{~A~}" x))
#
#  (signals-type-error x "abc" (format nil "~:@{~A~}" x))
#
#  (signals-error (format nil "~:@{~A ~A~}" '(x . y)) type-error)
#
#  "~{X ~A~^ Y ~A~^ ~}"
#
#  "~{X ~A~^ Y ~A~^ ~}"
#
#  "~1{~A~^~A~}"
#
#  "~0{~A~^~A~}"
#
#  "~1{~A~^~A~}"
#
#  "~{~A~A~0^~A~}"
#
#  "~{~A~A~v^~A~}"
#
#  "~{~#,3^~A~}"
#
#  "~{~#,#^~A~}"
#
#  "~{~#,#,#^~A~}"
#
#  "~{~#,1,2^~A~}"
#
#  "~{~#,#,v^~A~}"
#
#  "~{~v,v^~A~}"
#
#  "~{~0,v,v^~A~}"
#
#  "~{~1,v^~A~}"
#
#  "~{~0,v^~A~}"
#
#  "~{~1,2,v^~A~}"
#
#  "~{~1,1,v^~A~}"
#
#  "~{~'X^~A~}"
#
#  "~{~v,'X^~A~}"
#
#  "~{~'X,v^~A~}"
#
#  "~{~v,v^~A~}"
#
#  "~{~',,',^~A~}"
#
#  "~{~1,v,v^~A~}"
#
#  "~{~v,1,v^~A~}"
#
#  "~{~v,v,v^~A~}"
#
#  "~:{~A~^~A~A~}"
#
#  "~:{~A~0^~A~A~}"
#
#  "~:{~#^~A~}"
#
#  "~:{~#^~A~#^~A~#^~A~#^~A~}"
#
#  "~:{~v^~A~}"
#
#  "~:{~v,3^~A~}"
#
#  "~:{~3,v^~A~}"
#
#  "~:{~v,3^~A~}"
#
#  "~:{~2,v^~A~}"
#
#  "~:{~v,v^~A~}"
#
#  "~:{~'x,3^~A~}"
#
#  "~:{~3,'x^~A~}"
#
#  "~:{~'x,'x^~A~}"
#
#  "~:{~#,1^~A~}"
#
#  "~:{~1,#^~A~}"
#
#  "~:{~#,#^~A~}"
#
#  "~:{~0,v^~A~}"
#
#  "~:{~1,v^~A~}"
#
#  "~:{~1,1,1^~A~}"
#
#  "~:{~1,2,3^~A~}"
#
#  "~:{~1,2,1^~A~}"
#
#  "~:{~1,0,1^~A~}"
#
#  "~:{~3,2,1^~A~}"
#
#  "~:{~v,2,3^~A~}"
#
#  "~:{~1,v,3^~A~}"
#
#  "~:{~1,2,v^~A~}"
#
#  "~:{~1,2,v^~A~}"
#
#  "~:{~#,3,3^~A~}"
#
#  "~:{~2,#,3^~A~}"
#
#  "~:{~0,3,#^~A~}"
#
#  "~:{~#,#,3^~A~}"
#
#  "~:{~3,#,#^~A~}"
#
#  "~:{~#,3,#^~A~}"
#
#  "~:{~#,#,#^~A~}"
#
#  "~:{~1,v,v^~A~}"
#
#  "~:{~v,1,v^~A~}"
#
#  "~@{X ~A~^ Y ~A~^ ~}"
#
#  "~@{X ~A~^ Y ~A~^ ~}"
#
#  "~1@{~A~^~A~}"
#
#  "~0@{~A~^~A~}"
#
#  "~@{~A~A~0^~A~}"
#
#  "~@{~A~A~v^~A~}"
#
#  "~@{~#,3^~A~}"
#
#  "~@{~#,#^~A~}"
#
#  "~@{~#,#,#^~A~}"
#
#  "~@{~#,1,2^~A~}"
#
#  "~@{~#,#,v^~A~}"
#
#  "~@{~#,#,v^~A~}"
#
#  "~@{~#,#,v^~A~}"
#
#  "~@{~#,#,v^~A~}"
#
#  "~@{~#,#,v^~A~}"
#
#  "~@{~v,v^~A~}"
#
#  "~@{~0,v,v^~A~}"
#
#  "~@{~0,v,v^~A~}"
#
#  "~@{~1,v^~A~}"
#
#  "~@{~0,v^~A~}"
#
#  "~@{~1,2,v^~A~}"
#
#  "~@{~1,2,v^~A~}"
#
#  "~@{~1,1,v^~A~}"
#
#  "~@{~'X^~A~}"
#
#  "~@{~v,'X^~A~}"
#
#  "~@{~'X,v^~A~}"
#
#  "~@{~v,v^~A~}"
#
#  "~@{~',,',^~A~}"
#
#  "~@{~1,v,v^~A~}"
#
#  "~@{~v,1,v^~A~}"
#
#  "~@{~v,v,v^~A~}"
#
#  "~:@{~A~^~A~A~}"
#
#  "~@:{~A~0^~A~A~}"
#
#  "~:@{~#^~A~}"
#
#  "~@:{~#^~A~#^~A~#^~A~#^~A~}"
#
#  "~:@{~v^~A~}"
#
#  "~:@{~v^~A~}"
#
#  "~:@{~v^~A~}"
#
#  "~:@{~v,3^~A~}"
#
#  "~@:{~3,v^~A~}"
#
#  "~:@{~v,3^~A~}"
#
#  "~:@{~2,v^~A~}"
#
#  "~:@{~v,v^~A~}"
#
#  "~:@{~v,v^~A~}"
#
#  "~:@{~'x,3^~A~}"
#
#  "~:@{~3,'x^~A~}"
#
#  "~:@{~'x,'x^~A~}"
#
#  "~:@{~#,1^~A~}"
#
#  "~:@{~1,#^~A~}"
#
#  "~:@{~#,#^~A~}"
#
#  "~:@{~0,v^~A~}"
#
#  "~:@{~1,v^~A~}"
#
#  "~:@{~1,1,1^~A~}"
#
#  "~:@{~1,2,3^~A~}"
#
#  "~:@{~1,2,1^~A~}"
#
#  "~:@{~1,0,1^~A~}"
#
#  "~:@{~3,2,1^~A~}"
#
#  "~:@{~v,2,3^~A~}"
#
#  "~:@{~1,v,3^~A~}"
#
#  "~:@{~1,2,v^~A~}"
#
#  "~:@{~#,3,3^~A~}"
#
#  "~:@{~2,#,3^~A~}"
#
#  "~:@{~0,3,#^~A~}"
#
#  "~:@{~#,#,3^~A~}"
#
#  "~:@{~3,#,#^~A~}"
#
#  "~:@{~#,3,#^~A~}"
#
#  "~:@{~#,#,#^~A~}"
#
#  "~:@{~1,v,v^~A~}"
#
#  "~:@{~v,1,v^~A~}"
#
#  "~:{~:^~A~}"
#
#  "(~:{~A~:^,~})"
#
#  "~:{~:^~A~}"
#
#  "~:{~0:^~A~}"
#
#  "~:{~1:^~A~}"
#
#  "~:{~'X:^~A~}"
#
#  "~:{~v:^~A~}"
#
#  "~:{~V:^~A~}"
#
#  "~:{~#:^~A~}"
#
#  "~:{~1,1:^~A~}"
#
#  "~:{~0,1:^~A~}"
#
#  "~:{~v,1:^~A~}"
#
#  "~:{~1,V:^~A~}"
#
#  "~:{~V,v:^~A~}"
#
#  "~:{~#,1:^~A~}"
#
#  "~:{~1,#:^~A~}"
#
#  "~:{~#,#:^~A~}"
#
#  "~:{~#,v:^~A~}"
#
#  "~:{~V,#:^~A~}"
#
#  "~:{~'X,'Y:^~A~}"
#
#  "~:{~'X,'X:^~A~}"
#
#  "~:{~1,2,3:^~A~}"
#
#  "~:{~1,2,1:^~A~}"
#
#  "~:{~2,1,3:^~A~}"
#
#  "~:{~1,1,v:^~A~}"
#
#  "~:{~v,2,2:^~A~}"
#
#  "~:{~1,v,2:^~A~}"
#
#  "~:{~V,v,3:^~A~}"
#
#  "~:{~v,2,v:^~A~}"
#
#  "~:{~2,V,v:^~A~}"
#
#  "~:{~v,v,V:^~A~}"
#
#  "~:{~#,2,2:^~A~}" ('((1 2 3)(2 X X)(0 A B C D)(4 5)(5 7 8 9)))
#
#  "~:{~2,#,3:^~A~}" ('((1)(2 3 4 5)(3 4)(4 5 6 7 8)()))
#
#  "~:{~1,3,#:^~A~}" ('((1)(2 3)(3 4)(4 5 6)(5)))
#
#  "~:{~#,#,2:^~A~}" ('((1 2 3)(2 X X)(0 A B C D)(4 5)(5 7 8 9)))
#
#  "~:{~3,#,#:^~A~}" ('((1)(2 3)(3 4)(4 5 6)(5)))
#
#  "~:{~#,2,#:^~A~}" ('((1 2 3)(2)(0 A B C D)(4 5)(5 7 8 9)))
#
#  "~:{~#,#,#:^~A~}" ('((1 2 3)(2)(0 A B C D)(4 5)(5 7 8 9)))
#
#  "~:@{~:^~A~}" nil "")
#
#  "(~:@{~A~:^,~})" ('(1) '(2) '(3))
#
#  "~:@{~:^~A~}" ('(1) '(2) '(3) '(4))
#
#  "~:@{~0:^~A~}" ('(1) '(2))
#
#  "~:@{~1:^~A~}" ('(1) '(2))
#
#  "~:@{~'X:^~A~}" ('(1) '(2))
#
#  "~:@{~v:^~A~}" ('(1 8) '(2 3 4) '(3 1) '(0) '(6 7) '(8 10))
#
#  "~:@{~V:^~A~}" ('(#\X 1) '(0 2))
#
#  "~:@{~#:^~A~}" ('(1) '(2) '(3 4) '(5 6 7) () '(8 9 10))
#
#  "~:@{~1,1:^~A~}" (() '(1) '(2 3))
#
#  "~:@{~0,1:^~A~}" ('(1) '(2 3))
#
#  "~:@{~v,1:^~A~}" ('(2 3) '(4 5 6) '(0 2) '(1 7) '(9 10))
#
#  "~:@{~1,V:^~A~}" ('(2 3) '(4 5 6) '(0 2) '(1 7) '(9 10))
#
#  "~:@{~V,v:^~A~}" ('(0 1 2) '(1 0 3) '(4 4) () '(5 6 7))
#
#  "~:@{~#,1:^~A~}" ('(2 3 4) '(4 5) '(0) '(1 7) '(9 10))
#
#  "~:@{~1,#:^~A~}" ('(2 3 4) '(4 5) '(0) '(1 7) '(9 10))
#
#  "~:@{~#,#:^~A~}" (nil)
#
#  "~:@{~#,#:^~A~}" ('(1))
#
#  "~:@{~#,v:^~A~}" ('(1 2) '(3 4) '(2 5 6) '(1) '(2))
#
#  "~:@{~V,#:^~A~}" ('(0 2) '(1 3 4) '(1 3) () '(0 7))
#
#  "~:@{~'X,'Y:^~A~}" ('(1) '(2))
#
#  "~:@{~'X,'X:^~A~}" ('(1) '(2))
#
#  "~:@{~1,2,3:^~A~}" ('(1) '(2))
#
#  "~:@{~1,2,1:^~A~}"
#
#  "~:@{~2,1,3:^~A~}"
#
#  "~:@{~1,1,v:^~A~}"
#
#  "~:@{~v,2,2:^~A~}"
#
#  "~:@{~1,v,2:^~A~}"
#
#  "~:@{~V,v,3:^~A~}"
#
#  "~:@{~v,2,v:^~A~}"
#
#  "~:@{~2,V,v:^~A~}"
#
#  "~:@{~v,v,V:^~A~}"
#
#  "~:@{~#,2,2:^~A~}"
#
#  "~:@{~2,#,3:^~A~}"
#
#  "~:@{~1,3,#:^~A~}"
#
#  "~:@{~#,#,2:^~A~}"
#
#  "~:@{~3,#,#:^~A~}"
#
#  "~:@{~#,2,#:^~A~}"
#
#  "~:@{~#,#,#:^~A~}"
#
#  "~{~[X~;Y~;Z~;~0^~]~}"
#
#  "~{~[X~;Y~;Z~:;~0^~]~}"
#
#  "~{~[X~;Y~0^NO~;Z~;~^~]~}"
#
#  "~{~(~C~C~0^~C~)W~}"
#
#  "~{~:(~C~C~0^~C~)U~}"
#
#  "~{~@(~CA ~Cb ~0^~C~)V~}"
#
#  "~{~@:(~CA ~Cb ~0^~C~)W~}"
#
#  "~[~]"
#
#  "~[a~]"
#
#  "~[a~]"
#
#  "~[a~]"
#
#  "~[a~]" (1) "")
#
#  "~[a~]" ((1+ most-positive-fixnum)) "")
#
#        (format nil "~[a~;b~;c~;d~;e~;f~;g~;h~;i~]" i))
#
#  (let ((fn (formatter "~[a~;b~;c~;d~;e~;f~;g~;h~;i~]")))
#
#  "~0[a~;b~;c~;d~]"
#
#  "~-1[a~;b~;c~;d~]"
#
#  "~1[a~;b~;c~;d~]"
#
#  "~4[a~;b~;c~;d~]"
#
#  "~100000000000000000000000000000000[a~;b~;c~;d~]"
#
#        (format nil "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]" i nil))
#
#  (let ((fn (formatter "~V[a~;b~;c~;d~;e~;f~;g~;h~;i~]")))
#
#        (format nil "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]" nil i))
#
#  (let ((fn (formatter "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]")))
#
#  "~#[A~;B~]"
#
#  "~#[A~;B~]"
#
#        for s = (format nil "~[~:;a~]" i)
#
#  (let ((fn (formatter "~[~:;a~]")))
#
#  "~[a~:;b~]"
#
#  "~[a~:;b~]"
#
#  "~[a~:;b~]"
#
#        (format nil "~[a~;b~;c~;d~:;e~]" i))
#
#  (let ((fn (formatter "~[a~;b~;c~;d~:;e~]")))
#
#        (format nil "~v[a~;b~;c~;d~:;e~]" i nil))
#
#  (let ((fn (formatter "~v[a~;b~;c~;d~:;e~]")))
#
#        (format nil "~v[a~;b~;c~;d~:;e~]" nil i))
#
#  (let ((fn (formatter "~v[a~;b~;c~;d~:;e~]")))
#
#  "~#[A~:;B~]"
#
#  "~#[A~:;B~]"
#
#  "~:[a~;b~]"
#
#        for s = (format nil "~:[a~;b~]" x)
#
#  (let ((fn (formatter "~:[a~;b~]")))
#
#                (if mincol (format nil "~~~d," mincol) "~,")
#                (if padchar (format nil "'~c," padchar) ",")
#                (if commachar (format nil "'~c," commachar) ",")
#
#        (fn (formatter "~F")))
#     for s1 = (let ((*read-default-float-format* type)) (format nil "~f" x))
#
#        (fn (formatter "~f")))
#
#     for s1 = (let ((*read-default-float-format* type)) (format nil "~f" x))
#
#  (let ((fn (formatter "~3f")))
#          for s = (format nil "~3f" x)
#
#  (let ((fn (formatter "~2f")))
#          for s = (format nil "~2f" x)
#
#  (let ((fn (formatter "~4F")))
#          for s = (format nil "~4F" x)
#
#  (let ((fn (formatter "~4@F")))
#          for s = (format nil "~4@f" x)
#
#  (let ((fn (formatter "~3@F")))
#          for s = (format nil "~3@F" x)
#
#  (let ((fn (formatter "~4f")))
#          for s = (format nil "~4f" (- x))
#
#  (let ((fn (formatter "~3F")))
#          for s = (format nil "~3f" x)
#
#  (let ((fn (formatter "~4f")))
#          for s = (format nil "~4f" x)
#
#  (let ((fn (formatter "~4,2F")))
#          for s = (format nil "~4,2f" x)
#
#  (let ((fn (formatter "~3,2F")))
#          for s = (format nil "~3,2f" x)
#
#  (let ((fn (formatter "~2,1F")))
#          for s = (format nil "~2,1f" x)
#
#  (let ((fn (formatter "~4,2@F")))
#          for s = (format nil "~4,2@f" x)
#
#  (let ((fn (formatter "~2,2F")))
#          for s = (format nil "~2,2f" x)
#
#  (let ((fn (formatter "~,2F")))
#          for s = (format nil "~,2f" x)
#
#  (let ((fn (formatter "~,2F")))
#          for s = (format nil "~,2f" x)
#
#  (let ((fn (formatter "~4,2,-1F")))
#          for s = (format nil "~4,2,-1f" x)
#
#  (let ((fn (formatter "~4,2,0F")))
#          for s = (format nil "~4,2,0f" x)
#
#  (let ((fn (formatter "~4,2,1f")))
#          for s = (format nil "~4,2,1f" x)
#
#  (let ((fn (formatter "~5,1,,'*F")))
#          for s = (format nil "~5,1,,'*f" x)
#
#  (let ((fn (formatter "~5,1,,'*f")))
#          for s = (format nil "~5,1,,'*f" x)
#
#  (let ((fn (formatter "~4,0,,'*F")))
#          for s = (format nil "~4,0,,'*f" x)
#
#  (let ((fn (formatter "~1,1,,f")))
#          for s = (format nil "~1,1,,f" x)
#
#  (let ((fn (formatter "~10,1,,f")))
#          for s = (format nil "~10,1,,f" x)
#
#  (let ((fn (formatter "~10,1,,,'*F")))
#          for s = (format nil "~10,1,,,'*f" x)
#
#  (let ((fn (formatter "~VF")))
#          for s1 = (format nil "~f" x)
#          for s2 = (format nil "~vf" nil x)
#
#  (let ((fn (formatter "~,vf")))
#          for s1 = (format nil "~f" x)
#          for s2 = (format nil "~,vf" nil x)
#
#  (let ((fn (formatter "~,,Vf")))
#          for s1 = (format nil "~f" x)
#          for s2 = (format nil "~,,vf" nil x)
#
#  (let ((fn (formatter "~,,,vF")))
#          for s1 = (format nil "~f" x)
#          for s2 = (format nil "~,,,vf" nil x)
#
#  (let ((fn (formatter "~,,,,VF")))
#          for s1 = (format nil "~f" x)
#          for s2 = (format nil "~,,,,vf" nil x)
#          for s3 = (formatter-call-to-string fn nil x)
#
#           for s = (format nil "~f" sf)
#
#                      for s = (format nil "~v,vf" w d sf)
#
#           for s = (format nil "~v,vf" w d sf)
#
#           for s = (format nil "~v,vf" w d sf)
#
#           for s = (format nil "~v,vf" w d sf)
#
#     for s1 = (format nil f1 x)
#     for s2 = (format nil "~v,v,v,v,vf" w d k overflowchar padchar x)
#
#  "~,,,,',f" (0.0) "0.0")
#
#        for f1 = (and c (format nil "~~,,,,'~cf" c))
#        for s2 = (and c (format nil "~,,,,vf" c x))
#
#    "~2f" (1.1) "1.0")
#
#    "~3f" (1.1) "1.1")
#
#    "~0f" (0.01) ".0")
#
#    "~0,0f" (0.01) "0.")
#
#    "~3f" (0.000001) "0.0")
#
#    "~,,2f" (0.1) "10.0")
#
#  "~<M~3:i~:@_M~:>"
#  "~:<M~1:I~@:_M~:>"
#  "~<(~;M~-1:i~:@_M~;)~:>"
#  "~:<M~-1:i~:@_M~:>"
#  "~<(~;M~:I~:@_M~;)~:>"
#  "~<(~;M~v:i~:@_M~;)~:>"
#  "~:<M~-2:i~:@_M~:>"
#  "~<M~:i~:@_M~:>"
#  "~<MMM~I~:@_MMMMM~:>"
#  "~:<MMM~I~:@_MMMMM~:>"
#  "~<MMM~1I~:@_MMMMM~:>"
#  "XXX~<MMM~1I~:@_MMMMM~:>"
#  "XXX~<MMM~I~:@_MMMMM~:>"
#  "XXX~<MMM~-1I~:@_MMMMM~:>"
#  "XXX~<MMM~vI~:@_MMMMM~:>"
#  "XXX~<MMM~vI~:@_MMMMM~:>"
#  "~<~>"
#
#  "~<~A~>"
#  "~<~A~;~A~>"
#  "~,,1<~A~;~A~>"
#  "~,,1,',<~A~;~A~>"
#  "~,,2<~A~;~A~>"
#  "~v<~A~>"
#  "~v,,v<~A~>"
#  "~v,,,v<~A~>"
#  "~~~d,,,'~c<~~A~~>"
#  "~,v<~A~>"
#
#  "~<XXXXXX~^~>"
#  "~<XXXXXX~;YYYYYYY~^~>"
#  "~<~<XXXXXX~;YYYYYYY~^~>~>"
#  "~<XXXXXX~;YYYYYYY~^~;ZZZZZ~>"
#  "~13,,2<aaa~;bbb~;ccc~>"
#  "~10@<abcdef~>"
#  "~10:@<abcdef~>"
#  "~10:<abcdef~>"
#  "~4@<~>"
#  "~5:@<~>"
#  "~6:<~>"
#  "~v<~A~>"
#  "~,v<~A~;~A~>"
#  "~,,v<~A~;~A~>"
#  "~,,1,v<~A~;~A~>"
#  "~,,1,v<~A~;~A~>"
#  "~6<abc~;def~^~>"
#  "~6@<abc~;def~^~>"
#  "~%X ~,,1<~%X ~:;AAA~;BBB~;CCC~>"
#  "~%X ~<~%X ~0,3:;AAA~>~<~%X ~0,3:;BBB~>~<~%X ~0,3:;CCC~>"
#  "~%X ~<~%X ~0,30:;AAA~>~<~%X ~0,30:;BBB~>~<~%X ~0,30:;CCC~>"
#  "~%X ~<~%X ~0,3:;AAA~>,~<~%X ~0,3:;BBB~>,~<~%X ~0,3:;CCC~>"
#
#  (signals-error-always (format nil "~< ~W ~>" nil) error)
#  (signals-error-always (format nil "~<X~:;Y~>~W" nil) error)
#  (signals-error-always (format nil "~w~<X~:;Y~>" nil) error)
#  (signals-error-always (format nil "~< ~_ ~>") error)
#  (signals-error-always (format nil "~<X~:;Y~>~_") error)
#  (signals-error-always (format nil "~_~<X~:;Y~>") error)
#  (signals-error-always (format nil "~< ~i ~>") error)
#  (signals-error-always (format nil "~<X~:;Y~>~I") error)
#  (signals-error-always (format nil "~i~<X~:;Y~>") error)
#  (signals-error-always (format nil "~<foo~A~;~A~;bar~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~<foo~A~@;~A~;bar~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~<foo~;~A~;bar~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~<foo~@;~A~;bar~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~<foo~A~;~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~<foo~A~@;~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~<~;~A~;bar~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~<~@;~A~;bar~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~:<foo~A~;~A~;bar~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~:<foo~A~@;~A~;bar~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~:<foo~;~A~;bar~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~:<foo~@;~A~;bar~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~:<foo~A~;~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~:<foo~A~@;~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~:<~;~A~;bar~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~:<~@;~A~;bar~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~@<foo~A~;~A~;bar~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~@<foo~A~@;~A~;bar~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~@<foo~;~A~;bar~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~@<foo~@;~A~;bar~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~@<foo~A~;~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~@<foo~A~@;~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~@<~;~A~;bar~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "~@<~@;~A~;bar~A~:>" '(X) '(Y)) error)
#  (signals-error-always (format nil "1~<X~<Y~:>Z~>2" nil nil nil) error)
#  (signals-error-always (format nil "~<~:;~>~<~:>" nil nil nil) error)
#  (signals-error-always (format nil "~<~:>~<~:;~>" nil nil nil) error)
#
#  "~<~A~:>"
#  "~@<~A~:>"
#  "~:<~A~:>"
#  "~:@<~A~:>"
#  "~@:<~A~:>"
#  "~<~@{~A~^*~}~:>"
#  "~:<~@{~A~^*~}~:>"
#  "~<~;~A~;~:>"
#  "~<~;~A~:>"
#  "~@<~;~A~;~:>"
#  "~@<~;~A~:>"
#  "~:<[~;~@{~A~^/~}~:>"
#  "~:<~;~@{~A~^/~}~;]~:>"
#  "~:<[~;~@{~A~^/~}~;]~:>"
#  "~@<~@{~A~^*~}~:>"
#  "~@<~@{~A~^ ~_~}~:>"
#  "~:@<~@{~A~^ ~_~}~:>"
#  "~@:<~@{~A~^ ~}~:>"
#  "~@:<~@{~A~^ ~:_~}~:>"
#  "~:@<~@{~A~^ ~}~:@>"
#  (format nil "~:@<~@{~A~^/~
#                   ~}~:@>" 1 2 3)
#
#  "~:@<~@{~A~^            ~:_~}~:>"
#  "~:@<~@{~A~^            ~}~:@>"
#  "~:@<~@{~A~^~}~:@>"
#  "~@<**~@;~@{~A~^       ~}~:@>"
#  "~@<**~@;~@{~A~^       ~}~;XX~:@>"
#  "~:@<**~@;~@{~A~^       ~}~:@>"
#  "~:<~@{~A~^ ~}~:>"
#  "~<~A~^xxxx~:>"
#  "~<~<~A~^xxx~:>yyy~:>"
#
#  (let ((fn (formatter "~o")))
#    for s1 = (format nil "~O" i)
#
#  (let ((fn (formatter "~@O")))
#           for s1 = (format nil "~@o" i)
#
#         for s1 = (format nil "~o" i)
#         for fmt = (format nil "~~~do" mincol)
#
#         for s1 = (format nil "~o" i)
#         for fmt = (format nil "~~~do" mincol)
#
#         for s1 = (format nil "~@O" i)
#         for fmt = (format nil "~~~d@o" mincol)
#         for s2 = (format nil fmt i)
#
#         for s1 = (format nil "~@O" i)
#         for fmt = (format nil "~~~d@o" mincol)
#
#         for s1 = (format nil "~o" i)
#         for s2 = (format nil fmt i)
#
#         for s1 = (format nil "~o" i)
#
#  (let ((fn (formatter "~V,Vo")))
#           for s1 = (format nil "~o" i)
#           for s2 = (format nil "~v,vO" mincol padchar i)
#
#  (let ((fn (formatter "~v,V@O")))
#           for s1 = (format nil "~@o" i)
#           for s2 = (format nil "~v,v@o" mincol padchar i)
#
#  (let ((fn (formatter "~:O")))
#          for s1 = (format nil "~o" i)
#          for s2 = (format nil "~:o" i)
#
#  (let ((fn (formatter "~:o")))
#           for s1 = (format nil "~o" i)
#           for s2 = (format nil "~:O" i)
#
#  (let ((fn (formatter "~,,v:o")))
#           for s1 = (format nil "~o" i)
#           for s2 = (format nil "~,,v:o" commachar i)
#           for s3 = (formatter-call-to-string fn commachar i)
#
#         for s1 = (format nil "~o" i)
#         for s2 = (format nil fmt i)
#
#         for s1 = (format nil "~o" i)
#
#  (let ((fn (formatter "~,,V,v:O")))
#           for s1 = (format nil "~o" i)
#           for s2 = (format nil "~,,v,v:O" commachar commaint i)
#           for s3 = (formatter-call-to-string fn commachar commaint i)
#
#  (let ((fn (formatter "~,,v,V@:O")))
#           for s1 = (format nil "~@o" i)
#           for s2 = (format nil "~,,v,v:@o" commachar commaint i)
#
#  "~vO" (nil #o100) "100")
#
#  "~6,vO" (nil #o100) "   100")
#
#  "~,,v:o" (nil #o12345) "12,345")
#
#  "~,,'*,v:o" (nil #o12345) "12*345")
#
#          for s1 = (format nil "~o" x)
#
#  (let ((fn (formatter "~:o")))
#          for s1 = (format nil "~:o" x)
#          for s3 = (formatter-call-to-string fn x)
#
#  (let ((fn (formatter "~@o")))
#          for s1 = (format nil "~@o" x)
#          for s3 = (formatter-call-to-string fn x)
#
#  (let ((fn (formatter "~:@o")))
#          for s2 = (format nil "~@:o" x)
#
#   (let ((fn (formatter "~#o"))
#           for s = (apply #'format nil "~#o" n args)
#
#   (let ((fn (formatter "~,,,#:o"))
#           for s = (apply #'format nil "~,,,#:o" n args)
#
#   (let ((fn (formatter "~,,,#:@o"))
#           for s = (apply #'format nil "~,,,#@:O" n args)
#
#  "~+10o" (#o1234) "      1234")
#
#  "~+10@O" (#o1234) "     +1234")
#
#  "~-1O" (#o1234) "1234")
#
#  "~-1000000000000000000o" (#o1234) "1234")
#
#  "~vo" ((1- most-negative-fixnum) #o1234) "1234")
#
#  (let ((fn (formatter "~v,v,v,vo")))
#                (if mincol (format nil "~~~d," mincol) "~,")
#                (if padchar (format nil "'~c," padchar) ",")
#                (if commachar (format nil "'~c," commachar) ",")
#                (if commaint (format nil "~do" commaint) "o"))
#     for s1 = (format nil fmt x)
#     for s2 = (format nil "~v,v,v,vo" mincol padchar commachar commaint x)
#     for s3 = (formatter-call-to-string fn mincol padchar commachar commaint x)
#
#  "~0|" nil "")
#
#  (let ((s (format nil "~|")))
#              for s = (format nil (format nil "~~~D|" i))
#
#  (let ((s (format nil "~|")))
#              for s = (format nil "~v|" i)
#
#  "~V|" (0) "")
#
#  "~v|" (nil) #.(format nil "~|"))
#
#  "~(XXyy~AuuVV~)" ("ABc dEF ghI") "xxyyabc def ghiuuvv")
#
#                  (let ((s1 (format nil "~(~c~)" c))
#
#  (let ((fn (formatter "~(~c~)")))
#
#  "~@(this is a TEST.~)" nil "This is a test.")
#
#  "~@(!@#$%^&*this is a TEST.~)" nil "!@#$%^&*This is a test.")
#
#  "~:(this is a TEST.~)" nil "This Is A Test.")
#
#  "~:(this is7a TEST.~)" nil "This Is7a Test.")
#
#  "~:@(this is AlSo A teSt~)" nil "THIS IS ALSO A TEST")
#
#                  (let ((s1 (format nil "~@:(~c~)" c))
#
#  (let ((fn (formatter "~@:(~c~)")))
#
#  "~(aBc ~:(def~) GHi~)" nil "abc def ghi")
#
#  "~(aBc ~(def~) GHi~)" nil "abc def ghi")
#
#  "~@(aBc ~:(def~) GHi~)" nil "Abc def ghi")
#
#  "~(aBc ~@(def~) GHi~)" nil "abc def ghi")
#
#  "~(aBc ~:(def~) GHi~)" nil "abc def ghi")
#
#  "~:(aBc ~(def~) GHi~)" nil "Abc Def Ghi")
#
#  "~:(aBc ~:(def~) GHi~)" nil "Abc Def Ghi")
#
#  "~:(aBc ~@(def~) GHi~)" nil "Abc Def Ghi")
#
#  "~:(aBc ~@:(def~) GHi~)" nil "Abc Def Ghi")
#
#  "~@(aBc ~(def~) GHi~)" nil "Abc def ghi")
#
#  "~@(aBc ~:(def~) GHi~)" nil "Abc def ghi")
#
#  "~@(aBc ~@(def~) GHi~)" nil "Abc def ghi")
#
#  "~@(aBc ~@:(def~) GHi~)" nil "Abc def ghi")
#
#  "~:@(aBc ~(def~) GHi~)" nil "ABC DEF GHI")
#
#  "~@:(aBc ~:(def~) GHi~)" nil "ABC DEF GHI")
#
#  "~:@(aBc ~@(def~) GHi~)" nil "ABC DEF GHI")
#
#  "~@:(aBc ~@:(def~) GHi~)" nil "ABC DEF GHI")
#
#  "~%" nil #.(string #\Newline))
#
#        for format-string = (format nil "~~~D%" i)
#        for s2 = (format nil format-string)
#
#  "~v%" (nil) #.(string #\Newline))
#
#  "~V%" (1) #.(string #\Newline))
#
#        for s2 = (format nil "~v%" i)
#
#  (let ((fn (formatter "~v%")))
#
#        for s2 = (apply #'format nil "~#%" args)
#
#  (let ((fn (formatter "~#%")))
#
#  "~p" (1) "")
#
#  "~P" (2) "s")
#
#  "~p" (0) "s")
#
#  "~P" (1.0) "s")
#
#        for s = (format nil "~p" x)
#
#  (let ((fn (formatter "~p")))
#
#  "~D cat~:P" (1) "1 cat")
#
#  "~D cat~:p" (2) "2 cats")
#
#  "~D cat~:P" (0) "0 cats")
#
#  "~D cat~:p" ("No") "No cats")
#
#  "~D penn~:@P" (1) "1 penny")
#
#  "~D penn~:@p" (2) "2 pennies")
#
#  "~D penn~@:P" (0) "0 pennies")
#
#  "~D penn~@:p" ("No") "No pennies")
#
#  "~@p" (1) "y")
#
#  "~@P" (2) "ies")
#
#  "~@p" (0) "ies")
#
#  "~@P" (1.0) "ies")
#
#        for s = (format nil "~@p" x)
#
#  (let ((fn (formatter "~@P")))
#
#  "~?"
#  "a~?z"
#  "~@?"
#  "a~@?z"
#  "~{~A~@?~A~}"
#  "~~~dR"
#  "~2r"
#  "~3r"
#  "~~~D,~DR"
#  "~~~D,~D,'*r"
#        for s = (format nil "~vr" base (1+ base))
#
#  (let ((fn (formatter "~vr")))
#
#        for s1 = (format nil "~r" i)
#
#  "~vr" (nil 5) "five")
#
#  "~#r" (4 nil nil) "11" 2)
#
#     (format nil "~10r" 123)))
#
#  (let ((fn (formatter "~10r")))
#
#  "~8@R" (65) "+101")
#
#  "~2:r" (126) "1,111,110")
#
#  "~3@:r" (#3r2120012102) "+2,120,012,102")
#
#   for s = (format nil "~~~d:R" i)
#
#   for s = (format nil "~~~d,,,'~c,~d:R" i comma interval)
#
#  "~2,,,,1000000000000000000r" (17) "10001")
#
#  "~8,10:@r" (#o526104) "  +526,104")
#
#        for s1 = (format nil "~:r" i)
#
#  (let ((fn (formatter "~:r")))
#
#        for s1 = (format nil "~:r" (- i))
#
#        for s2 = (format nil "~@R" i)
#
#  (let ((fn (formatter "~@r")))
#
#        for s1 = (format nil "~:@r" i)
#
#  (let ((fn (formatter "~@:R")))
#
#        for s1 = (format nil "~:@r" i)
#        for s2 = (format nil "~@:R" i)
#
#  "~2,12,,'*:r" (#b1011101) "   1*011*101")
#
#  "~3,14,'X,',:R" (#3r1021101) "XXXXX1,021,101")
#
#  "~10,vr" (nil 12345) "12345")
#
#        for s = (format nil "~10,vr" i 12345)
#
#  (let ((fn (formatter "~10,vr")))
#
#  "~10,#r" (12345 nil nil nil nil nil) " 12345" 5)
#
#  "~10,12,vr" (#\/ 123456789) "///123456789")
#
#  "~10,,,v:r" (#\/ 123456789) "123/456/789")
#
#  "~10,,,v:r" (nil 123456789) "123,456,789")
#
#  "~8,,,,v:R" (nil #o12345670) "12,345,670")
#
#  "~8,,,,v:R" (2 #o12345670) "12,34,56,70")
#
#  "~16,,,,#:r" (#x12345670 nil nil nil) "1234,5670" 3)
#
#  "~16,,,,1:r" (#x12345670) "1,2,3,4,5,6,7,0")
#
#  "~+10r" (12345) "12345")
#
#  "~10,+8r" (12345) "   12345")
#
#  "~10,0r" (12345) "12345")
#
#  "~10,-1r" (12345) "12345")
#
#  "~10,-1000000000000000r" (12345) "12345")
#
#  (let ((fn (formatter "~v,v,v,v,vr")))
#                (format nil "~~~d," radix)
#                (if mincol (format nil "~d," mincol) ",")
#                (if padchar (format nil "'~c," padchar) ",")
#                (if commachar (format nil "'~c," commachar) ",")
#                (if commaint (format nil "~dr" commaint) "r"))
#     for s2 = (format nil "~v,v,v,v,vr" radix mincol padchar commachar commaint x)
#
#  (format nil "~/pprint-linear/" 1)
#
#  (format nil "~/pprint-linear/" 2)
#
#  (format nil "~/pprint-linear/" '(17))
#
#  (format nil "~:/pprint-linear/" '(17))
#
#  (format nil "~@/pprint-linear/" 1)
#
#  (format nil "~@:/pprint-linear/" 1)
#
#  (format nil "~/PPRINT-LINEAR/" 1)
#
#  (format nil "~/pPrINt-lINeaR/" 1)
#
#    (format nil "~/CL-TEST::FUNCTION-FOR-FORMAT-SLASH-9/" 1))
#
#    (format nil "~/cl-test:FUNCTION-FOR-FORMAT-SLASH-10/" 1))
#
#    (format nil "~/cL-tESt:FUNCTION:FOR::FORMAT:SLASH:11/" 1))
#
#  (format nil "~<~/pprint-tabular/~:>" '((|M|)))
#
#  (format nil "~<~:/pprint-tabular/~:>" '((|M|)))
#
#  (format nil "~<~:@/pprint-tabular/~:>" '((|M|)))
#
#  (format nil "~<~@/pprint-tabular/~:>" '((|M|)))
#
#  (format nil "~<~4:/pprint-tabular/~:>" '((|M| |M|)))
#
#  (format nil "~<~v:/pprint-tabular/~:>" '(nil (|M| |M|)))
#
#  (format nil "~<~v:/pprint-tabular/~:>" '(3 (|M| |M|)))
#
#      (format nil "~/cl-test::function-for-format-slash-19/" 1))
#      (format nil "~:/cl-test::function-for-format-slash-19/" 2))
#      (format nil "~@/cl-test::function-for-format-slash-19/" 3))
#      (format nil "~:@/cl-test::function-for-format-slash-19/" 4))
#      (format nil "~@:/cl-test::function-for-format-slash-19/" 5))
#      (format nil "~18@:/cl-test::function-for-format-slash-19/" 6))
#      (format nil "~v/cl-test::function-for-format-slash-19/" 19 7))
#      (format nil "~'X:/cl-test::function-for-format-slash-19/" 8))
#      (format nil "~',@/cl-test::function-for-format-slash-19/" 9))
#      (format nil "~-1@/cl-test::function-for-format-slash-19/" 10))
#      (format nil "~1,2,3,4,5,6,7,8,9,10@/cl-test::function-for-format-slash-19/" 11))
#      (format nil "~v,v,v,v,v,v,v,v,v,v@/cl-test::function-for-format-slash-19/" 1 2 3 4 5 6 7 8 9 10 12))
#    ))
#
#    (format nil "~s" nil))
#
#    (formatter-call-to-string (formatter "~s") nil))
#
#  "~:s" (nil) "()")
#
#    (format nil "~:s" '(nil)))
#
#    (formatter-call-to-string (formatter "~:s") '(nil)))
#
#    (format nil "~s" 'nil))
#
#    (formatter-call-to-string (formatter "~s") 'nil))
#
#    (format nil "~s" 'nil))
#
#    (formatter-call-to-string (formatter "~s") 'nil))
#
#  "~:s" (#(nil)) "#(NIL)")
#
#             for s = (format nil "~S" c)
#
#  (let ((fn (formatter "~s")))
#             for s2 = (and c (format nil "~S" c))
#
#            for fmt = (format nil "~~~d@s" i)
#
#            for fmt = (format nil "~~~dS" i)
#
#            for fmt = (format nil "~~~d@:S" i)
#
#            for fmt = (format nil "~~~d:s" i)
#
#         (fn (formatter "~V:s")))
#            for s = (format nil "~v:S" i nil)
#
#         (fn (formatter "~V@:s")))
#            for s = (format nil "~v:@s" i nil)
#
#  "~vS" (nil nil) "NIL")
#
#  "~v:S" (nil nil) "()")
#
#  "~@S" (nil) "NIL")
#
#  "~v@S" (nil nil) "NIL")
#
#  "~v:@s" (nil nil) "()")
#
#  "~v@:s" (nil nil) "()")
#
#  "~3,1s" (nil) "NIL")
#
#  "~4,3s" (nil) "NIL   ")
#
#  "~3,3@s" (nil) "NIL")
#
#  "~4,4@s" (nil) "    NIL")
#
#  "~5,3@s" (nil) "   NIL")
#
#  "~5,3S" (nil) "NIL   ")
#
#  "~7,3@s" (nil) "      NIL")
#
#  "~7,3S" (nil) "NIL      ")
#
#         (fn (formatter "~V,,2s")))
#           for s = (format nil "~v,,2S" i 'ABC)
#
#  "~3,,+2S"
#
#  "~3,,0S"
#
#  "~3,,-1S"
#
#  "~3,,0S"
#
#  "~3,,-1S"
#
#  "~4,,,'XS"
#
#  "~4,,,s"
#
#  "~4,,,'X@s"
#
#  "~4,,,@S"
#
#  "~10,,,vS"
#
#  "~10,,,v@S"
#
#  "~10,,,vs"
#
#  "~10,,,v@s"
#
#  "~3,,vS"
#
#         (fn (formatter "~3,,vs")))
#           for s = (format nil "~3,,vS" i 'ABC)
#
#         (fn (formatter "~3,,V@S")))
#           for s = (format nil "~3,,v@S" i 'ABC)
#
#  "~4,,vs" (-1 1234) "1234")
#
#  "~5,vS" (nil 123) "123  ")
#
#  "~5,vS" (3 456) "456   ")
#
#  "~5,v@S" (3 789) "   789")
#
#  "~~" nil "~")
#
#        for format-string = (format nil "~~~D~~" i)
#
#  "~v~" (0) "")
#
#        for s2 = (format nil "~V~" i)
#
#  (let ((fn (formatter "~v~")))
#
#        for s2 = (apply #'format nil "~#~" args)
#
#  (format nil "~0,0T")
#
#  (format nil "~1,0T")
#
#  (format nil "~0,1T")
#
#        for s = (format nil "~0,vT" i)
#
#        for s = (format nil "~v,0T" i)
#
#                       (format nil (format nil "~A~~~D,~DT" s1 n2 inc)))
#
#                       (format nil "~A~v,vt" s1 n2 inc))
#
#        for s = (format nil " ~v,vT" nil i)
#
#  (format nil "XXXXX~2,0T")
#
#  (format nil "~1,1@t")
#
#        for s1 = (format nil "~v,1@t" colnum)
#
#        for s1 = (format nil "~v,v@t" colnum colinc)
#
#        for s1 = (format nil "~v,1@T~0,v@t" colnum colinc)
#
#                   (format nil (format nil "~~~d,~d@t" colnum colinc)))
#
#  (format nil "XX~10:tYY")
#
#    (format s "XX~10:tYY")))
#
#      (format s "XX~10:tYY"))))
#
#  (format nil "~<[~;~0,0:T~;]~:>" '(a))
#
#  (format nil "~<[~;~1,0:T~;]~:>" '(a))
#
#  (format nil "~<[~;~,0:T~;]~:>" '(a))
#
#  (format nil "~<[~;~0,1:T~;]~:>" '(a))
#
#  (format nil "~<[~;~0,:T~;]~:>" '(a))
#
#  (format nil "~<[~;~0:T~;]~:>" '(a))
#
#        for s = (format nil "~<X~;~0,v:T~;Y~:>" (list i))
#
#        for s = (format nil "~<ABC~;~v,0:T~;DEF~:>" (list i))
#
#   for result = (format nil (format nil "~A~~<~A~~~D,~D:T~~:>" s0 s1 n2 inc) '(a))
#
#  (format nil "~<[~;~2,0:T~;]~:>" '(a))
#
#  (format nil "~<[~;XXXX~2,0:T~;]~:>" '(a))
#
#        for result = (format nil "~A~<~A~v,v:t~:>" s0 (list s1 n2 inc))
#
#  (signals-error-always (format nil "~<XXX~1,1:TYYY~>") error)
#
#  (signals-error-always (format nil "~<XXX~:;YYY~>ZZZ~4,5:tWWW") error)
#
#  (signals-error-always (format nil "AAAA~1,1:TBBB~<XXX~:;YYY~>ZZZ") error)
#
#  (format nil "~<XXX~;~1,1:@t~;YYY~:>" '(a))
#
#  (format nil "~<XXX~;~,1:@t~;YYY~:>" '(a))
#
#  (format nil "~<XXX~;~1,:@t~;YYY~:>" '(a))
#
#  (format nil "~<XXX~;~1:@t~;YYY~:>" '(a))
#
#  (format nil "~<XXX~;~:@t~;YYY~:>" '(a))
#
#        for s1 = (format nil "~<XXXX~;~v,1:@t~:>" (list colnum))
#
#        for s1 = (format nil "~A~<~v,v:@t~:>" s0 (list colnum colinc))
#
#  (format nil "XX~10,20:@tYY")
#
#    (format s "XX~10,20@:tYY")))
#
#    (format t "B ~_")
#
#   (format t "B ~_")
#   (format t "D ~_")
#
#  (format t "A ~_A ~_A ~_A ~_")
#
#  (format t "A ~_A ~_A ~_A ~_")
#
#  (format t "A ~_A ~_A ~_A ~_A ~_A ~_A ~_A ~_A ~_A ~_")
#
#  (dotimes (i 4) (format t "A             ~_"))
#
#  (format t "A ~_A ~_A ~_A ~_~%A ~_A ~_A ~_A ~_")
#
#                          (format t "A ~_A ~_A ~_A ~_"))
#                          (format t "A ~_A ~_A ~_A ~_")))
#
#       (format s "A ~_A ~_A ~_A ~_A ~_"))))
#
#      (formatter "A ~_A ~_A ~_A ~_A ~_"))))
#
#  (format t "A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_")
#
#  (format t "A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_")
#
#  (format t "A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_")
#
#  (format t "A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_")
#
#  (format t "A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_")
#
#  (format t "~%A~@_")
#
#  (format t "~@_A~%")
#
#    (format t "AAAA ~_")
#     (format t "A ~@_A ~@_A ~@_A ~@_")))
#
#    (format t "AAAA ~:@_")
#
#       (format s "A ~@_A ~@_A ~@_A ~@_A ~@_"))))
#
#  (format t "A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_")
#
#  (format t "~W~W~:_~W~W~:_~W~W~:_~W~W~:_~W~W~:_"
#
#       (format s "A ~:_A ~:_A ~:_A ~:_A ~:_"))))
#
#  (format t "A ~:@_A ~:@_A ~:@_A ~:@_")
#
#  (format t "A ~@:_A ~@:_A ~@:_A ~@:_")
#
#  (format t "A ~@:_A ")
#
#  (format t "A ~@:_A ~@:_A ~@:_A ~@:_")
#
#       (format s "A ~:@_A ~:@_A ~:@_A ~:@_A ~:@_"))))
#
#     (formatter-call-to-string (formatter "A ~:@_A ~:@_A ~:@_A ~:@_A ~:@_"))))
#
#  (let ((fn (formatter "~x")))
#           for s1 = (format nil "~X" i)
#
#  (let ((fn (formatter "~@X")))
#           for s1 = (format nil "~@x" i)
#
#         for s1 = (format nil "~x" i)
#         for s2 = (format nil fmt i)
#
#         for s1 = (format nil "~x" i)
#
#         for s1 = (format nil "~@X" i)
#         for s2 = (format nil fmt i)
#
#         for s1 = (format nil "~@X" i)
#
#         for s1 = (format nil "~x" i)
#
#         for s1 = (format nil "~x" i)
#
#  (let ((fn (formatter "~V,vx")))
#           for s1 = (format nil "~x" i)
#           for s2 = (format nil "~v,vX" mincol padchar i)
#
#  (let ((fn (formatter "~v,V@X")))
#           for s1 = (format nil "~@x" i)
#           for s2 = (format nil "~v,v@x" mincol padchar i)
#
#  (let ((fn (formatter "~:X")))
#          for s1 = (format nil "~x" i)
#          for s2 = (format nil "~:x" i)
#
#  (let ((fn (formatter "~:x")))
#           for s1 = (format nil "~x" i)
#           for s2 = (format nil "~:X" i)
#
#  (let ((fn (formatter "~,,V:x")))
#           for s1 = (format nil "~x" i)
#           for s2 = (format nil "~,,v:X" commachar i)
#
#         for s1 = (format nil "~x" i)
#         for s2 = (format nil fmt i)
#
#         for s1 = (format nil "~x" i)
#
#  (let ((fn (formatter "~,,v,v:X")))
#           for s1 = (format nil "~x" i)
#           for s2 = (format nil "~,,v,v:X" commachar commaint i)
#
#  (let ((fn (formatter "~,,v,V:@x")))
#           for s1 = (format nil "~@x" i)
#           for s2 = (format nil "~,,v,v:@x" commachar commaint i)
#
#  "~vx" (nil #x100) "100")
#
#  "~6,vX" (nil #x100) "   100")
#
#  "~,,v:x" (nil #x12345) "12,345")
#
#  "~,,'*,v:x" (nil #x12345) "12*345")
#
#  (let ((fn (formatter "~x")))
#          for s1 = (format nil "~x" x)
#
#  (let ((fn (formatter "~:x")))
#          for s1 = (format nil "~:x" x)
#
#  (let ((fn (formatter "~@x")))
#          for s1 = (format nil "~@x" x)
#
#  (let ((fn (formatter "~:@x")))
#          for s2 = (format nil "~@:x" x)
#
#   (let ((fn (formatter "~#X"))
#           for s = (apply #'format nil "~#x" n args)
#
#   (let ((fn (formatter "~,,,#:X"))
#           for s = (apply #'format nil "~,,,#:x" n args)
#
#   (let ((fn (formatter "~,,,#@:X"))
#           for s = (apply #'format nil "~,,,#@:X" n args)
#
#  "~+10x" (#x1234) "      1234")
#
#  "~+10@X" (#x1234) "     +1234")
#
#  "~-1X" (#x1234) "1234")
#
#  "~-1000000000000000000x" (#x1234) "1234")
#
#  "~vx" ((1- most-negative-fixnum) #x1234) "1234")
#
#  (let ((fn (formatter "~v,v,v,vx")))
#                (if mincol (format nil "~~~d," mincol) "~,")
#                (if padchar (format nil "'~c," padchar) ",")
#                (if commachar (format nil "'~c," commachar) ",")
#                (if commaint (format nil "~dx" commaint) "x"))
#     for s2 = (format nil "~v,v,v,vx" mincol padchar commachar commaint x)
