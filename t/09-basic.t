use v6;

use Test;
use lib 't/lib';
use Utils;
use Format::Lisp;

my $fl = Format::Lisp.new;

#`(
subtest {

#`(
	is $fl.format( Q{} ), Q{}, Q{no arguments};
)

#`(
	subtest {
#`(
		is $fl.format( Q{}, 1 ), Q{}, Q{number};
)
#`(
		is $fl.format( Q{}, 'foo' ), Q{}, Q{string};
)
#`(
		is $fl.format( Q{}, $fl.formatter( Q{} ) ), Q{}, Q{formatter};
)
	}, Q{unused arguments};
)
}, Q{empty format};
)

subtest {

	subtest {

		is $fl.format( Q{a} ), Q{a}, Q{ASCII};
		is $fl.format( Q{Ø} ), Q{Ø}, Q{Latin-1};
		is $fl.format( Q{ऄ} ), Q{ऄ}, Q{Devanagari};
	}, Q{no arguments};

	subtest {

		is $fl.format( Q{a}, 1 ), Q{a}, Q{ASCII};
		is $fl.format( Q{Ø}, 2, Q{foo} ), Q{Ø}, Q{Latin-1};
		is $fl.format( Q{ऄ}, 3, $fl.formatter( "" ) ), Q{ऄ}, Q{Devanagari};
	}, Q{unused arguments};

}, Q{text, no directives};

subtest {
	is $fl.format( qq{~\n} ), Q{}, 'newline';

	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~$} ), Q{}, 'no arguments';
		} X::Format-Error;
)

		is $fl.format( Q{~$}, Q{} ), Q{}, 'empty argument';
	}, 'dollar';

	is $fl.format( Q{~%} ), qq{\n}, 'percent';

	is $fl.format( Q{~&} ), Q{}, 'ampersand';

	is $fl.format( Q{~(~)} ), Q{}, 'parentheses';

	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~*} ), Q{}, 'no arguments';
		} X::Format-Error;
)

		is $fl.format( Q{~*}, Q{} ), Q{}, 'one argument';
	}, 'asterisk (goto)';

	is $fl.format( Q{~<~>} ), Q{}, 'angles';

	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~;} ), Q{}, 'bare';
		} X::Format-Error;

		throws-ok {
			is $fl.format( Q{~(~;~)} ), Q{}, 'not inside [] <>';
		} X::Format-Error;

		throws-ok {
			is $fl.format( Q{~{~;~}} ), Q{}, 'not inside [] <>';
		} X::Format-Error;
)

		is $fl.format( Q{~<~;~>} ), Q{}, 'inside angles';
	}, 'semicolon';

	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~?} ), Q{}, 'no arguments';
		} X::Format-Error;
)

		is $fl.format( Q{~?}, Q{}, Nil ), Q{}, 'one argument';
	}, 'question';

#`(
	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~A} ), Q{}, 'no arguments';
		} X::Format-Error;
)

#`(
		is $fl.format( Q{~A}, Q{} ), Q{}, 'one argument';
)
	}, 'A';
)

#`(
	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~B} ), Q{}, 'no arguments';
		} X::Format-Error;
)

#`(
		is $fl.format( Q{~B}, Q{} ), Q{}, 'one argument';
)
	}, 'B';
)

#`(
	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~C} ), Q{}, 'No arguments';
		} X::Format-Error;

		throws-ok {
			is $fl.format( Q{~C} ), Q{foo}, 'bad type';
		} X::Format-Error;
)

#`(
		is $fl.format( Q{~C}, Q{} ), Q{}, 'C';
)
	}, 'C';
)

#`(
	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~D} ), Q{}, 'no arguments';
		} X::Format-Error;
)

#`(
		is $fl.format( Q{~D}, Q{} ), Q{}, 'one argument';
)
	}, 'D';
)

	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~E} ), Q{}, 'no arguments';
		} X::Format-Error;
)

		is $fl.format( Q{~E}, Q{} ), Q{}, 'one argument';
	}, 'E';

#`(
	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~F} ), Q{}, 'no arguments';
		} X::Format-Error;
)

#`(
		is $fl.format( Q{~F}, Q{} ), Q{}, 'one argument';
)
	}, 'F';
)

	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~G} ), Q{}, 'no arguments';
		} X::Format-Error;
)

		is $fl.format( Q{~G}, Q{} ), Q{}, 'one argument';
	}, 'G';

#`(
	throws-ok {
		is $fl.format( Q{~H} ), Q{}, 'H invalid';
	} X::Format-Error;
)

	# ~H doesn't exist

	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~I} ), Q{}, 'no arguments';
		} X::Format-Error;
)

		is $fl.format( Q{~I}, Q{} ), Q{}, 'one argument';
	}, 'I';

#`(
	throws-ok {
		is $fl.format( Q{~J} ), Q{}, 'J invalid';
	} X::Format-Error;

	throws-ok {
		is $fl.format( Q{~K} ), Q{}, 'K invalid';
	} X::Format-Error;

	throws-ok {
		is $fl.format( Q{~L} ), Q{}, 'L invalid';
	} X::Format-Error;

	throws-ok {
		is $fl.format( Q{~M} ), Q{}, 'M invalid';
	} X::Format-Error;

	throws-ok {
		is $fl.format( Q{~N} ), Q{}, 'N invalid';
	} X::Format-Error;
)

#`(
	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~O} ), Q{}, 'no arguments';
		} X::Format-Error;
)

#`(
		is $fl.format( Q{~O}, Q{} ), Q{}, 'one argument';
)
	}, 'O';
)

#`(
	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~P} ), Q{}, 'no arguments';
		} X::Format-Error;
)

#`(
		is $fl.format( Q{~P}, Q{} ), Q{s}, 'one argument';
)
	}, 'P';
)

#`(
	throws-ok {
		is $fl.format( Q{~Q} ), Q{}, 'Q invalid';
	} X::Format-Error;
)

	# ~Q does not exist

#`(
	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~R} ), Q{}, 'no arguments';
		} X::Format-Error;

		throws-ok {
			is $fl.format( Q{~R}, Q{} ), Q{}, 'wrong type';
		} X::Format-Error;
)

#`(
		is $fl.format( Q{~R}, Q{1} ), Q{1}, 'one argument';
)
	}, 'R';
)

#`(
	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~S} ), Q{}, 'no arguments';
		} X::Format-Error;
)

#`(
		is $fl.format( Q{~S}, Q{} ), Q{""}, 'one argument';
)
	}, 'S';
)

#`(
	is $fl.format( Q{~T} ), Q{ }, 'T';
)

#`(
	throws-ok {
		is $fl.format( Q{~U} ), Q{}, 'U invalid';
	} X::Format-Error;
)

	# ~U does not exist
#`(
	throws-ok {
		is $fl.format( Q{~V} ), Q{}, 'V invalid';
	} X::Format-Error;
)

	# ~V is just plain weird. Claims unterminated string.
	# Maybe Unicode completion?

#`(
	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~W} ), Q{}, 'no arguments';
		} X::Format-Error;
)

#`(
		is $fl.format( Q{~W}, Q{} ), Q{""}, 'one argument';
)
	}, 'W';
)

#`(
	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~X} ), Q{}, 'no arguments';
		} X::Format-Error;
)

#`(
		is $fl.format( Q{~X}, Q{} ), Q{}, 'one argument';
)
	}, 'X';
)

#`(
	throws-ok {
		is $fl.format( Q{~Y} ), Q{}, 'Y invalid';
	} X::Format-Error;

	throws-ok {
		is $fl.format( Q{~Z} ), Q{}, 'Z invalid';
	} X::Format-Error;
)

	subtest {
#`(
		throws-ok {
			is $fl.format( Q{~[~]} ), Q{}, 'no arguments';
		} X::Format-Error;
)

		is $fl.format( Q{~[~]}, Q{} ), Q{}, 'one argument';
	}, '[]';


	is $fl.format( Q{~^} ), Q{}, '^';

	is $fl.format( Q{~_} ), Q{}, '_';
#`(
	throws-ok {
		is $fl.format( Q{~`} ), Q{}, '` invalid';
	} X::Format-Error;
)

	# XXX *not* reiterating the lower-case letters here...

	subtest {
#`(
		throws-ok {
			is $fl.format( Q[~\{~\}] ), Q{}, 'no arguments';
		} X::Format-Error;
)

		is $fl.format( Q[~{~}], Q{} ), Q{}, 'one argument';
	}, '\{\}';

#`(
	is $fl.format( Q{~|} ), qq{\n }, '|';
)

	is $fl.format( Q{~~} ), Q{~}, '~';

}, Q{single unnested directive, in ASCII order};

done-testing;

# vim: ft=perl6
