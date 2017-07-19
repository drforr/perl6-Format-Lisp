use v6;

use Test;
use lib 't/lib';
use Utils;
use Format::Lisp;

my $fl = Format::Lisp.new;

#`(
# (defun def-format-test (name args result)
#   `(deftest ,name
#      (equalt
#       (with-standard-io-syntax
#        (with-output-to-string (s) (format s ,@args)))
#       result)
#      t))
# 
)

done-testing;
