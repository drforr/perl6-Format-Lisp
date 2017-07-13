use v6;

use Test;
use Format::Lisp;

# We're just checking odds and ends here, so no need to rigorously check
# the object tree.

my $fl = Format::Lisp.new;
my $*CONSISTENCY-CHECK = True;
my $*FALL-THROUGH = True;

done-testing; # Because we're going to be adding tests quite often.

# vim: ft=perl6
