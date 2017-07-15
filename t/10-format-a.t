use v6;

use Test;
use Format::Lisp;

my $fl = Format::Lisp.new;

my @standard-chars =
	split( '', Q{abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789~!@#$%^&*()_+|\\=-`{}[]:\";'<>?,./} ), "\n";

# (def-format-test format.a.1
#   "~a" (nil) "NIL")
# 
is $fl.format( Q{~a}, Nil ), Q{NIL}, 'format.a.1';

# (deftest format.a.2
#   (with-standard-io-syntax
#    (let ((*print-case* :downcase))
#      (format nil "~A" nil)))
#   "nil")
# 
subtest {
	my $*PRINT-CASE = 'downcase';
	is $fl.format( Q{~A}, Nil ), Q{nil};
}, 'format.a.2';

# (deftest formatter.a.2
#   (with-standard-io-syntax
#    (let ((*print-case* :downcase))
#      (formatter-call-to-string (formatter "~A") nil)))
#   "nil")
# 
subtest {
	my $*PRINT-CASE = 'downcase';
	is $fl.formatter-call-to-string(
		$fl.formatter( Q{~A} ), Nil
	), Q{nil};
}, 'formatter.a.2';

# (deftest format.a.3
#   (with-standard-io-syntax
#    (let ((*print-case* :capitalize))
#      (format nil "~a" nil)))
#   "Nil")
# 
subtest {
	my $*PRINT-CASE = 'capitalize';
	is $fl.format( Q{~a}, Nil ), Q{Nil};
}, 'format.a.3';

# (deftest formatter.a.3
#   (with-standard-io-syntax
#    (let ((*print-case* :capitalize))
#      (formatter-call-to-string (formatter "~a") nil)))
#   "Nil")
# 
# (def-format-test format.a.4
#   "~:a" (nil) "()")
# 
is $fl.format( Q{~:a}, Nil ), Q{()}, 'format.a.4';

#`(
# (def-format-test format.a.5
#   "~:A" ('(nil)) "(NIL)")
# 
is $fl.format( Q{~:A}, [ Nil ] ), Q{(NIL)}, 'format.a.5';
)

# (def-format-test format.a.6
#   "~:A" (#(nil)) "#(NIL)")
# 
# Maybe this would be []?

# (deftest format.a.7
#   (let ((fn (formatter "~a")))
#     (loop for c across +standard-chars+
#           for s1 = (string c)
#           for s2 = (format nil "~a" s1)
#           for s3 = (formatter-call-to-string fn s1)
#           unless (and (string= s1 s2) (string= s2 s3))
#           collect (list c s1 s2 s3)))
#   nil)
# 
subtest {
	my $fn = $fl.formatter( Q{~a} );
	my @collected;
	for @standard-chars -> $c {
		my $s1 = $c;
		my $s2 = $fl.format( Q{~a}, $s1 );
		my $s3 = $fl.formatter-call-to-string( $fn, $s1 );
		unless $s1 eq $s1 and $s2 eq $s3 {
			@collected.append( [ $c, $s1, $s2, $s3 ] );
		}
	}
	is @collected.elems, 0;
}, 'format.a.7';

#`(
# (deftest format.a.8
#   (let ((fn (formatter "~A")))
#     (loop with count = 0
#           for i from 0 below (min #x10000 char-code-limit)
#           for c = (code-char i)
#           for s1 = (and c (string c))
#           for s2 = (and c (format nil "~A" s1))
#           for s3 = (and c (formatter-call-to-string fn s1))
#           unless (or (null c) (string= s1 s2) (string= s2 s3))
#           do (incf count) and collect (list c s1 s2 s3)
#           when (> count 100) collect "count limit exceeded" and do (loop-finish)))
#   nil)
# 
)

#`(
# (deftest format.a.9
#   (with-standard-io-syntax
#    (apply
#     #'values
#     (loop for i from 1 to 10
#           for fmt = (format nil "~~~d@a" i)
#           for s = (format nil fmt nil)
#           for fn = (eval `(formatter ,fmt))
#           for s2 = (formatter-call-to-string fn nil)
#           do (assert (string= s s2))
#           collect s)))
#   "NIL"
#   "NIL"
#   "NIL"
#   " NIL"
#   "  NIL"
#   "   NIL"
#   "    NIL"
#   "     NIL"
#   "      NIL"
#   "       NIL")
# 
subtest {
	my @collected;
	for 1 .. 10 -> $i {
		my $fmt = $fl.format( Q{~~~d@a}, $i );
		my $s = $fl.format( $fmt, Nil );
		my $fn = $fl.formatter( $fmt );
		my $s2 = $fl.formatter-call-to-string( $fn, Nil );
		is $s, $s2;
		@collected.append( $s );
	}
	is-deeply @collected, [
		"NIL",
		"NIL",
		"NIL",
		" NIL",
		"  NIL",
		"   NIL",
		"    NIL",
		"     NIL",
		"      NIL",
		"       NIL"
	];
}, 'format.a.9';
)

#`(
# (deftest format.a.10
#   (with-standard-io-syntax
#    (apply
#     #'values
#     (loop for i from 1 to 10
#           for fmt = (format nil "~~~da" i)
#           for s = (format nil fmt nil)
#           for fn = (eval `(formatter ,fmt))
#           for s2 = (formatter-call-to-string fn nil)
#           do (assert (string= s s2))
#           collect s)))
#   "NIL"
#   "NIL"
#   "NIL"
#   "NIL "
#   "NIL  "
#   "NIL   "
#   "NIL    "
#   "NIL     "
#   "NIL      "
#   "NIL       ")
# 
subtest {
	my @collected;
	for 1 .. 10 -> $i {
		my $fmt = $fl.format( Q{~~~da}, $i );
		my $s = $fl.format( $fmt, Nil );
		my $fn = $fl.formatter( $fmt );
		my $s2 = $fl.formatter-call-to-string( $fn, Nil );
		is $s, $s2;
		@collected.append( $s );
	}
	is-deeply @collected, [
		"NIL",
		"NIL",
		"NIL",
		"NIL ",
		"NIL  ",
		"NIL   ",
		"NIL    ",
		"NIL     ",
		"NIL      ",
		"NIL       "
	];
}, 'format.a.10';
)

#`(
# (deftest format.a.11
#   (with-standard-io-syntax
#    (apply
#     #'values
#     (loop for i from 1 to 10
#           for fmt = (format nil "~~~d@:A" i)
#           for s = (format nil fmt nil)
#           for fn = (eval `(formatter ,fmt))
#           for s2 = (formatter-call-to-string fn nil)
#           do (assert (string= s s2))
#           collect s)))
#   "()"
#   "()"
#   " ()"
#   "  ()"
#   "   ()"
#   "    ()"
#   "     ()"
#   "      ()"
#   "       ()"
#   "        ()")
# 
subtest {
	my $collected;
	for 1 .. 10 -> $i {
		my $fmt = $fl.format( Q{~~~d@:A}, $i );
		my $s = $fl.format( $fmt, Nil );
		my $fn = $fl.formatter( $fmt );
		my $s2 = $fl.formatter-call-to-string( $fn, Nil );
		is $s, $s2;
		@collected.append( $s );
	}
	is-deeply $collected, [
		"()",
		"()",
		" ()",
		"  ()",
		"   ()",
		"    ()",
		"     ()",
		"      ()",
		"       ()",
		"        ()"
	];
}, 'format.a.11';
)

#`(
# (deftest format.a.12
#   (with-standard-io-syntax
#    (apply
#     #'values
#     (loop for i from 1 to 10
#           for fmt = (format nil "~~~d:a" i)
#           for s = (format nil fmt nil)
#           for fn = (eval `(formatter ,fmt))
#           for s2 = (formatter-call-to-string fn nil)
#           do (assert (string= s s2))
#           collect s)))
#   "()"
#   "()"
#   "() "
#   "()  "
#   "()   "
#   "()    "
#   "()     "
#   "()      "
#   "()       "
#   "()        ")
# 
subtest {
	my $collected;
	for 1 .. 10 -> $i {
		my $fmt = $fl.format( Q{~~~d:a}, $i );
		my $s = $fl.format( $fmt, Nil );
		my $fn = $fl.formatter( $fmt );
		my $s2 = $fl.formatter-call-to-string( $fn, Nil );
		is $s, $s2;
		@collected.append( $s );
	}
	is-deeply $collected, [
		"()",
		"()",
		"() ",
		"()  ",
		"()   ",
		"()    ",
		"()     ",
		"()      ",
		"()       ",
		"()        "
	];
}, 'format.a.12';
)

#`(
# (deftest format.a.13
#   (with-standard-io-syntax
#    (apply
#     #'values
#     (let ((fn (formatter "~V:a")))
#       (loop for i from 1 to 10
#             for s = (format nil "~v:A" i nil)
#             for s2 = (formatter-call-to-string fn i nil)
#             do (assert (string= s s2))
#             collect s))))
#   "()"
#   "()"
#   "() "
#   "()  "
#   "()   "
#   "()    "
#   "()     "
#   "()      "
#   "()       "
#   "()        ")
# 
subtest {
	my $collected;
	my $fn = $fl.formatter( Q{~V:a} );
	for 1 .. 10 -> $i {
		my $s = $fl.format( Q{~v:A}, $i, Nil );
		my $s2 = $fl.formatter-call-to-string( $fn $i, Nil );
		is $s, $s2;
		@collected.append( $s );
	}
	is-deeply $collected, [
		"()",
		"()",
		"() ",
		"()  ",
		"()   ",
		"()    ",
		"()     ",
		"()      ",
		"()       ",
		"()        "
	];
}, 'format.a.13';
)

#`(
# (deftest format.a.14
#   (with-standard-io-syntax
#    (apply
#     #'values
#     (let ((fn (formatter "~V@:A")))
#       (loop for i from 1 to 10
#             for s = (format nil "~v:@a" i nil)
#             for s2 = (formatter-call-to-string fn i nil)
#             do (assert (string= s s2))
#             collect s))))
#   "()"
#   "()"
#   " ()"
#   "  ()"
#   "   ()"
#   "    ()"
#   "     ()"
#   "      ()"
#   "       ()"
#   "        ()")
# 
subtest {
	my $collected;
	my $fn = $fl.formatter( Q{~V@:A} );
	for 1 .. 10 -> $i {
		my $s = $fl.format( Q{~v:@A}, $i, Nil );
		my $s2 = $fl.formatter-call-to-string( $fn $i, Nil );
		is $s, $s2;
		@collected.append( $s );
	}
	is-deeply $collected, [
		"()",
		"()",
		" ()",
		"  ()",
		"   ()",
		"    ()",
		"     ()",
		"      ()",
		"       ()",
		"        ()",
	];
}, 'format.a.14';
)

# (def-format-test format.a.15
#   "~vA" (nil nil) "NIL")
# 
is $fl.format( Q{~vA}, Nil, Nil ), "NIL", 'format.a.15';

# (def-format-test format.a.16
#   "~v:A" (nil nil) "()")
# 
is $fl.format( Q{~v:A}, Nil, Nil ), "()", 'format.a.16';

# (def-format-test format.a.17
#   "~@A" (nil) "NIL")
# 
is $fl.format( Q{~@A}, Nil ), "NIL", 'format.a.17';

# (def-format-test format.a.18
#   "~v@A" (nil nil) "NIL")
# 
is $fl.format( Q{~v@A}, Nil, Nil ), "NIL", 'format.a.18';

# (def-format-test format.a.19
#   "~v:@a" (nil nil) "()")
# 
is $fl.format( Q{~v:@a}, Nil, Nil ), "()", 'format.a.19';

# (def-format-test format.a.20
#   "~v@:a" (nil nil) "()")
# 
is $fl.format( Q{~v@:a}, Nil, Nil ), "()", 'format.a.20';

# ;;; With colinc specified
# 
# (def-format-test format.a.21
#   "~3,1a" (nil) "NIL")
# 
is $fl.format( Q{~3,1a}, Nil ), "NIL", 'format.a.21';

# (def-format-test format.a.22
#   "~4,3a" (nil) "NIL   ")
# 
is $fl.format( Q{~4,3a}, Nil ), "NIL   ", 'format.a.22';

# (def-format-test format.a.23
#   "~3,3@a" (nil) "NIL")
# 
is $fl.format( Q{~3,3@A}, Nil ), "NIL", 'format.a.23';

# (def-format-test format.a.24
#   "~4,4@a" (nil) "    NIL")
# 
is $fl.format( Q{~4,4@A}, Nil ), "    NIL", 'format.a.24';

# (def-format-test format.a.25
#   "~5,3@a" (nil) "   NIL")
# 
is $fl.format( Q{~5,3@A}, Nil ), "   NIL", 'format.a.25';

# (def-format-test format.a.26
#   "~5,3A" (nil) "NIL   ")
# 
is $fl.format( Q{~5,3A}, Nil ), "NIL   ", 'format.a.26';

# (def-format-test format.a.27
#   "~7,3@a" (nil) "      NIL")
# 
is $fl.format( Q{~7,3@A}, Nil ), "      NIL", 'format.a.27';

# (def-format-test format.a.28
#   "~7,3A" (nil) "NIL      ")
# 
is $fl.format( Q{~7,3A}, Nil ), "NIL      ", 'format.a.28';

#`(
# ;;; With minpad
# 
# (deftest format.a.29
#   (let ((fn (formatter "~v,,2A")))
#     (loop for i from -4 to 10
#           for s = (format nil "~v,,2A" i "ABC")
#           for s2 = (formatter-call-to-string fn i "ABC")
#           do (assert (string= s s2))
#           collect s))
#   ("ABC  "
#    "ABC  "
#    "ABC  "
#    "ABC  "
#    "ABC  "
#    "ABC  "
#    "ABC  "
#    "ABC  "
#    "ABC  "
#    "ABC  "
#    "ABC   "
#    "ABC    "
#    "ABC     "
#    "ABC      "
#    "ABC       "))
# 
subtest {
	my $collected;
	my $fn = $fl.formatter( Q{~v,,2A} );
	for -4 .. 10 -> $i {
		my $s = $fl.format( Q{~v,,2A}, $i, 'ABC' );
		my $s2 = $fl.formatter-call-to-string( $fn, $i, 'ABC' );
		is $s, $s2;
		$collected.append( $s );
	}
	is-deeply $collected, [
		"ABC  ",
		"ABC  ",
		"ABC  ",
		"ABC  ",
		"ABC  ",
		"ABC  ",
		"ABC  ",
		"ABC  ",
		"ABC  ",
		"ABC  ",
		"ABC   ",
		"ABC    ",
		"ABC     ",
		"ABC      ",
		"ABC       "
	];
}, 'format.a.29';
)

# (def-format-test format.a.30
#   "~3,,+2A" ("ABC") "ABC  ")
# 
is $fl.format( Q{~3,,+2A}, 'ABC' ), "ABC  ", 'format.a.30';

# (def-format-test format.a.31
#   "~3,,0A" ("ABC") "ABC")
# 
is $fl.format( Q{~3,,0A}, 'ABC' ), "ABC", 'format.a.31';

# (def-format-test format.a.32
#   "~3,,-1A" ("ABC") "ABC")
# 
is $fl.format( Q{~3,,-1A}, 'ABC' ), "ABC", 'format.a.32';

# (def-format-test format.a.33
#   "~3,,0A" ("ABCD") "ABCD")
# 
is $fl.format( Q{~3,,0A}, 'ABCD' ), "ABCD", 'format.a.33';

# (def-format-test format.a.34
#   "~3,,-1A" ("ABCD") "ABCD")
# 
is $fl.format( Q{~3,,-1A}, 'ABCD' ), "ABCD", 'format.a.34';

# ;;; With padchar
# 
# (def-format-test format.a.35
#   "~4,,,'XA" ("AB") "ABXX")
# 
is $fl.format( Q{~4,,,'XA}, 'AB' ), "ABXX", 'format.a.35';

# (def-format-test format.a.36
#   "~4,,,a" ("AB") "AB  ")
# 
is $fl.format( Q{~4,,,a}, 'AB' ), "AB  ", 'format.a.36';

# (def-format-test format.a.37
#   "~4,,,'X@a" ("AB") "XXAB")
# 
is $fl.format( Q{~4,,,'X@a}, 'AB' ), "XXAB", 'format.a.37';

# (def-format-test format.a.38
#   "~4,,,@A" ("AB") "  AB")
# 
is $fl.format( Q{~4,,,@A}, 'AB' ), "  AB", 'format.a.38';

#`(
# (def-format-test format.a.39
#   "~10,,,vA" (nil "abcde") "abcde     ")
# 
is $fl.format( Q{~10,,,vA}, Nil, 'abcde' ), "abcde     ", 'format.a.39';
)

#`(
# (def-format-test format.a.40
#   "~10,,,v@A" (nil "abcde") "     abcde")
# 
is $fl.format( Q{~10,,,v@a}, Nil, 'abcde' ), "     abcde", 'format.a.40';
)

#`(
# (def-format-test format.a.41
#   "~10,,,va" (#\* "abcde") "abcde*****")
# 
is $fl.format( Q{~10,,,va}, Q{*}, 'abcde' ), "abcde*****", 'format.a.41';
)

#`(
# (def-format-test format.a.42
#   "~10,,,v@a" (#\* "abcde") "*****abcde")
# 
is $fl.format( Q{~10,,,v@a}, Q{*}, 'abcde' ), "*****abcde", 'format.a.42';
)

# ;;; Other tests
# 
# (def-format-test format.a.43
#   "~3,,vA" (nil "ABC") "ABC")
# 
is $fl.format( Q{~3,,va}, Nil, 'ABC' ), "ABC", 'format.a.43';

#`(
# (deftest format.a.44
#   (let ((fn (formatter "~3,,vA")))
#     (loop for i from 0 to 6
#           for s =(format nil "~3,,vA" i "ABC")
#           for s2 = (formatter-call-to-string fn i "ABC")
#           do (assert (string= s s2))
#           collect s))
#   ("ABC"
#    "ABC "
#    "ABC  "
#    "ABC   "
#    "ABC    "
#    "ABC     "
#    "ABC      "))
# 
subtest {
	my $collected;
	my $fn = $fl.formatter( Q{~3,,vA} );
	for ^6 -> $i {
		my $s = $fl.format( Q{~3,,vA}, $i, 'ABC' );
		my $s2 = $fl.formatter-call-to-string( $fn, $i, 'ABC' );
		is $s, $s2;
		$collected.append( $s );
	}
	is-deeply $collected, [
		"ABC",
		"ABC ",
		"ABC  ",
		"ABC   ",
		"ABC    ",
		"ABC     ",
		"ABC      "
	];
}, 'format.a.44a';
)

#`(
# (deftest format.a.44a
#   (let ((fn (formatter "~3,,v@A")))
#     (loop for i from 0 to 6
#           for s = (format nil "~3,,v@A" i "ABC")
#           for s2 = (formatter-call-to-string fn i "ABC")
#           do (assert (string= s s2))
#           collect s))
#   ("ABC"
#    " ABC"
#    "  ABC"
#    "   ABC"
#    "    ABC"
#    "     ABC"
#    "      ABC"))
# 
subtest {
	my $collected;
	my $fn = $fl.formatter( Q{~3,,v@A} );
	for ^6 -> $i {
		my $s = $fl.format( Q{~3,,v@A}, $i, 'ABC' );
		my $s2 = $fl.formatter-call-to-string( $fn, $i, 'ABC' );
		is $s, $s2;
		$collected.append( $s );
	}
	is-deeply $collected, [
		"ABC",
		" ABC",
		"  ABC",
		"   ABC",
		"    ABC",
		"     ABC",
		"      ABC"
	];
}, 'format.a.44a';
)

#`(
# (def-format-test format.a.45
#   "~4,,va" (-1 "abcd") "abcd")
# 
is $fl.format( Q{~4,,va}, -1, 'abcd' ), "abcd", 'format.a.45';
)

#`(
# (def-format-test format.a.46
#   "~5,vA" (nil "abc") "abc  ")
# 
is $fl.format( Q{~5,vA}, Nil, 'abc' ), "abc  ", 'format.a.46';
)

#`(
# (def-format-test format.a.47
#   "~5,vA" (3 "abc") "abc   ")
# 
is $fl.format( Q{~5,vA}, 3, 'abc' ), "abc   ", 'format.a.47';
)

#`(
# (def-format-test format.a.48
#   "~5,v@A" (3 "abc") "   abc")
# 
is $fl.format( Q{~5,v@A}, 3, 'abc' ), "   abc", 'format.a.48';
)

#`(
# ;;; # parameters
# 
# (def-format-test format.a.49
#   "~#A" ("abc" nil nil nil) "abc " 3)
# 
is $fl.format( Q{~#A}, 'abc', Nil, Nil, Nil ), "abc ", 'format.a.49';
)

#`(
# (def-format-test format.a.50
#   "~#@a" ("abc" nil nil nil nil nil) "   abc" 5)
# 
is $fl.format( Q{~#@a}, 'abc', Nil, Nil, Nil, Nil, Nil ),
	"   abc",
	'format.a.50'
;
)

# (def-format-test format.a.51
#   "~5,#a" ("abc" nil nil nil) "abc    " 3)
# 
is $fl.format( Q{~5,#a}, 'abc', Nil, Nil, Nil ), "abc    ", 'format.a.51';

# (def-format-test format.a.52
#   "~5,#@A" ("abc" nil nil nil) "    abc" 3)
# 
is $fl.format( Q{~5,#@A}, 'abc', Nil, Nil, Nil ), "    abc", 'format.a.52';

# (def-format-test format.a.53
#   "~4,#A" ("abc" nil nil) "abc   " 2)
# 
is $fl.format( Q{~4,#A}, 'abc', Nil, Nil ), "abc   ", 'format.a.53';

# (def-format-test format.a.54
#   "~4,#@A" ("abc" nil nil) "   abc" 2)
# 
is $fl.format( Q{~4,#@A}, 'abc', Nil, Nil ), "   abc", 'format.a.54';

# (def-format-test format.a.55
#   "~#,#A" ("abc" nil nil nil) "abc    " 3)
# 
is $fl.format( Q{~#,#A}, 'abc', Nil, Nil, Nil ), "abc    ", 'format.a.55';

# (def-format-test format.a.56
#   "~#,#@A" ("abc" nil nil nil) "    abc" 3)
# 
is $fl.format( Q{~#,#@A}, 'abc', Nil, Nil, Nil ), "    abc", 'format.a.56';

# (def-format-test format.a.57
#   "~-100A" ("xyz") "xyz")
# 
is $fl.format( Q{~-10@A}, 'xyz' ), 'xyz', 'format.a.57';

# (def-format-test format.a.58
#   "~-100000000000000000000a" ("xyz") "xyz")
#
is $fl.format( Q{~-100000000000000000000a}, 'xyz' ), "xyz", 'format.a.58';

done-testing;