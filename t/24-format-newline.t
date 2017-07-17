use v6;

use Test;
use Format::Lisp;

my $fl = Format::Lisp.new;

#`(
# (def-format-test format.newline.1
#   (concatenate 'string "~" (string #\Newline) "   X")
#   nil "X")
# 
)

#`(
# (def-format-test format.newline.2
#   (concatenate 'string "A~:" (string #\Newline) " X")
#   nil "A X")
# 
)

#`(
# (def-format-test format.newline.3
#   (concatenate 'string "A~@" (string #\Newline) " X")
#   nil #.(concatenate 'string "A" (string #\Newline) "X"))
# 
)

done-testing;
