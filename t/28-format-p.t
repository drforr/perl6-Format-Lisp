use v6;

use Test;
use lib 't/lib';
use Utils;
use Format::Lisp;

my $fl = Format::Lisp.new;

# (def-format-test format.p.1
#   "~p" (1) "")
# 
is $fl.format( Q{~p}, 1 ), Q{}, 'format.p.1';

# (def-format-test format.p.2
#   "~P" (2) "s")
# 
is $fl.format( Q{~P}, 2 ), Q{s}, 'format.p.2';

# (def-format-test format.p.3
#   "~p" (0) "s")
# 
is $fl.format( Q{~p}, 2 ), Q{s}, 'format.p.3';

# (def-format-test format.p.4
#   "~P" (1.0) "s")
# 
is $fl.format( Q{~P}, 2 ), Q{s}, 'format.p.4';

#`(
# (deftest format.p.5
#   (loop for x in *universe*
#         for s = (format nil "~p" x)
#         unless (or (eql x 1) (string= s "s"))
#         collect (list x s))
#   nil)
# 
is do {
	my @collected;
	for 0 .. 10 -> $i {
#		my @args = 
##		my $s = $fl.format( Q{~v,,2A}, $i, 'ABC' );
#		my $s2 = $fl.formatter-call-to-string( $fn, $i, 'ABC' );
#		is $s, $s2;
#		@collected.append( $s );
	}
	@collected.elems;
}, 0, 'format.p.5';
)

#`(
# (deftest formatter.p.5
#   (let ((fn (formatter "~p")))
#     (loop for x in *universe*
#           for s = (formatter-call-to-string fn x)
#           unless (or (eql x 1) (string= s "s"))
#           collect (list x s)))
#   nil)
# 
is do {
	my $fn = $fl.formatter( Q{~p} );
	my @collected;
	for 0 .. 10 -> $i {
#		my @args = 
##		my $s = $fl.format( Q{~v,,2A}, $i, 'ABC' );
#		my $s2 = $fl.formatter-call-to-string( $fn, $i, 'ABC' );
#		is $s, $s2;
#		@collected.append( $s );
	}
	@collected.elems;
}, 0, 'formatter.p.5';
)

# ;;; :p
# 
#`(
# (def-format-test format.p.6
#   "~D cat~:P" (1) "1 cat")
# 
is $fl.format( Q{~O cat~:P}, 1 ), Q{1 cat}, 'format.p.6';
)

#`(
# (def-format-test format.p.7
#   "~D cat~:p" (2) "2 cats")
# 
is $fl.format( Q{~O cat~:P}, 2 ), Q{2 cats}, 'format.p.7';
)

#`(
# (def-format-test format.p.8
#   "~D cat~:P" (0) "0 cats")
# 
is $fl.format( Q{~O cat~:P}, 0 ), Q{0 cats}, 'format.p.8';
)

#`(
# (def-format-test format.p.9
#   "~D cat~:p" ("No") "No cats")
# 
is $fl.format( Q{~O cat~:P}, 'No' ), Q{No cats}, 'format.p.9';
)

# ;;; :@p
# 
#`(
# (def-format-test format.p.10
#   "~D penn~:@P" (1) "1 penny")
# 
is $fl.format( Q{~O penn~:P}, 1 ), Q{1 penny}, 'format.p.10';
)

#`(
# (def-format-test format.p.11
#   "~D penn~:@p" (2) "2 pennies")
# 
is $fl.format( Q{~O penn~:@P}, 2 ), Q{2 pennies}, 'format.p.11';
)

#`(
# (def-format-test format.p.12
#   "~D penn~@:P" (0) "0 pennies")
# 
is $fl.format( Q{~O penn~:@P}, 0 ), Q{0 pennies}, 'format.p.12';
)

#`(
# (def-format-test format.p.13
#   "~D penn~@:p" ("No") "No pennies")
# 
is $fl.format( Q{~O penn~@:P}, 'No' ), Q{No pennies}, 'format.p.13';
)

# ;;; @p
# 
#`(
# (def-format-test format.p.14
#   "~@p" (1) "y")
# 
is $fl.format( Q{~@p}, 1 ), Q{y}, 'format.p.14';
)

#`(
# (def-format-test format.p.15
#   "~@P" (2) "ies")
# 
is $fl.format( Q{~@p}, 2 ), Q{ies}, 'format.p.15';
)

#`(
# (def-format-test format.p.16
#   "~@p" (0) "ies")
# 
is $fl.format( Q{~@p}, 0 ), Q{ies}, 'format.p.16';
)

#`(
# (def-format-test format.p.17
#   "~@P" (1.0) "ies")
# 
is $fl.format( Q{~@P}, 1.0 ), Q{ies}, 'format.p.17';
)

#`(
# (deftest format.p.18
#   (loop for x in *universe*
#         for s = (format nil "~@p" x)
#         unless (or (eql x 1) (string= s "ies"))
#         collect (list x s))
#   nil)
# 
is do {
	my @collected;
	for 0 .. 10 -> $i {
#		my @args = 
##		my $s = $fl.format( Q{~v,,2A}, $i, 'ABC' );
#		my $s2 = $fl.formatter-call-to-string( $fn, $i, 'ABC' );
#		is $s, $s2;
#		@collected.append( $s );
	}
	@collected.elems;
}, 0, 'format.p.18';
)

#`(
# (deftest formatter.p.18
#   (let ((fn (formatter "~@P")))
#     (loop for x in *universe*
#           for s = (formatter-call-to-string fn x)
#           unless (or (eql x 1) (string= s "ies"))
#           collect (list x s)))
#   nil)
# 
is do {
	my @collected;
	for 0 .. 10 -> $i {
#		my @args = 
##		my $s = $fl.format( Q{~v,,2A}, $i, 'ABC' );
#		my $s2 = $fl.formatter-call-to-string( $fn, $i, 'ABC' );
#		is $s, $s2;
#		@collected.append( $s );
	}
	@collected.elems;
}, 0, 'formatter.p.18';
)

done-testing;

# vim: ft=perl6
