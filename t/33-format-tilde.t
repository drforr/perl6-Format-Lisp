use v6;

use Test;
use lib 't/lib';
use Utils;
use Format::Lisp;

my $fl = Format::Lisp.new;

# (def-format-test format.~.1
#   "~~" nil "~")
# 
is $fl.format( Q{~~}), Q{~}, 'format.~.1';

#`(
# (deftest format.~.2
#   (loop for i from 0 to 100
#         for s = (make-string i :initial-element #\~)
#         for format-string = (format nil "~~~D~~" i)
#         for s2 = (format nil format-string)
#         unless (string= s s2)
#         collect (list i s s2))
#   nil)
# 
is do {
	my @collected;
	for 0 .. 100 -> $i {
#		my @args = 
##		my $s = $fl.format( Q{~v,,2A}, $i, 'ABC' );
#		my $s2 = $fl.formatter-call-to-string( $fn, $i, 'ABC' );
#		is $s, $s2;
#		@collected.append( $s );
	}
	@collected.elems;
}, 0, 'format.~.2';
)

#`(
# (deftest formatter.~.2
#   (loop for i from 0 to 100
#         for s = (make-string i :initial-element #\~)
#         for format-string = (format nil "~~~D~~" i)
#         for fn = (eval `(formatter ,format-string))
#         for s2 = (formatter-call-to-string fn)
#         unless (string= s s2)
#         collect (list i s s2))
#   nil)
# 
is do {
	my @collected;
	for 0 .. 100 -> $i {
#		my @args = 
##		my $s = $fl.format( Q{~v,,2A}, $i, 'ABC' );
#		my $s2 = $fl.formatter-call-to-string( $fn, $i, 'ABC' );
#		is $s, $s2;
#		@collected.append( $s );
	}
	@collected.elems;
}, 0, 'formatter.~.2';
)

#`(
# (def-format-test format.~.3
#   "~v~" (0) "")
# 
is $fl.format( Q{~v~}, 0), Q{}, 'format.~.3';
)

#`(
# (deftest format.~.4
#   (loop for i from 0 to 100
#         for s = (make-string i :initial-element #\~)
#         for s2 = (format nil "~V~" i)
#         unless (string= s s2)
#         collect (list i s s2))
#   nil)
# 
is do {
	my @collected;
	for 0 .. 100 -> $i {
#		my @args = 
##		my $s = $fl.format( Q{~v,,2A}, $i, 'ABC' );
#		my $s2 = $fl.formatter-call-to-string( $fn, $i, 'ABC' );
#		is $s, $s2;
#		@collected.append( $s );
	}
	@collected.elems;
}, 0, 'format.~.4';
)

#`(
# (deftest formatter.~.4
#   (let ((fn (formatter "~v~")))
#     (loop for i from 0 to 100
#           for s = (make-string i :initial-element #\~)
#           for s2 = (formatter-call-to-string fn i)
#           unless (string= s s2)
#           collect (list i s s2)))
#   nil)
# 
is do {
	my @collected;
	for 0 .. 100 -> $i {
#		my @args = 
##		my $s = $fl.format( Q{~v,,2A}, $i, 'ABC' );
#		my $s2 = $fl.formatter-call-to-string( $fn, $i, 'ABC' );
#		is $s, $s2;
#		@collected.append( $s );
	}
	@collected.elems;
}, 0, 'formatter.~.4';
)

#`(
# (deftest format.~.5
#   (loop for i from 0 to (min (- call-arguments-limit 3) 100)
#         for s = (make-string i :initial-element #\~)
#         for args = (make-list i)
#         for s2 = (apply #'format nil "~#~" args)
#         unless (string= s s2)
#         collect (list i s s2))
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
}, 0, 'format.~.5';
)

#`(
# (deftest formatter.~.5
#   (let ((fn (formatter "~#~")))
#     (loop for i from 0 to (min (- call-arguments-limit 3) 100)
#           for s = (make-string i :initial-element #\~)
#           for args = (make-list i)
#           for s2 = (with-output-to-string
#                      (stream)
#                      (assert (equal (apply fn stream args) args)))
#           unless (string= s s2)
#           collect (list i s s2)))
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
}, 0, 'formatter.~.5';
)

done-testing;

# vim: ft=perl6
