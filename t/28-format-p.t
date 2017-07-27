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
)

# ;;; :p
# 
#`(
# (def-format-test format.p.6
#   "~D cat~:P" (1) "1 cat")
# 
)

#`(
# (def-format-test format.p.7
#   "~D cat~:p" (2) "2 cats")
# 
)

#`(
# (def-format-test format.p.8
#   "~D cat~:P" (0) "0 cats")
# 
)

#`(
# (def-format-test format.p.9
#   "~D cat~:p" ("No") "No cats")
# 
)

# ;;; :@p
# 
#`(
# (def-format-test format.p.10
#   "~D penn~:@P" (1) "1 penny")
# 
)

#`(
# (def-format-test format.p.11
#   "~D penn~:@p" (2) "2 pennies")
# 
)

#`(
# (def-format-test format.p.12
#   "~D penn~@:P" (0) "0 pennies")
# 
)

#`(
# (def-format-test format.p.13
#   "~D penn~@:p" ("No") "No pennies")
# 
)

# ;;; @p
# 
#`(
# (def-format-test format.p.14
#   "~@p" (1) "y")
# 
)

#`(
# (def-format-test format.p.15
#   "~@P" (2) "ies")
# 
)

#`(
# (def-format-test format.p.16
#   "~@p" (0) "ies")
# 
)

#`(
# (def-format-test format.p.17
#   "~@P" (1.0) "ies")
# 
)

#`(
# (deftest format.p.18
#   (loop for x in *universe*
#         for s = (format nil "~@p" x)
#         unless (or (eql x 1) (string= s "ies"))
#         collect (list x s))
#   nil)
# 
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
)

done-testing;

# vim: ft=perl6
