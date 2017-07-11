use v6;

use Test;
use Format::Lisp;
plan 8;

my $fl = Format::Lisp.new;
my $*CONSISTENCY-CHECK = True;
my $*FALL-THROUGH = True;

# Ack. I think running the permutations here will be simpler than trying to
# match all the tests here, which really aren't full coverage. I'll leave the
# original tests here but commented out for a little while though.

subtest {
	is-deeply $fl._parse( Q{~a} ), [ Format::Lisp::Directive::A.new ];
	is-deeply $fl._parse( Q{~A} ), [ Format::Lisp::Directive::A.new ];
	is-deeply $fl._parse( Q{~&} ), [ Format::Lisp::Directive::Amp.new ];
	is-deeply $fl._parse( Q{~b} ), [ Format::Lisp::Directive::B.new ];
	is-deeply $fl._parse( Q{~B} ), [ Format::Lisp::Directive::B.new ];
	is-deeply $fl._parse( Q{~c} ), [ Format::Lisp::Directive::C.new ];
	is-deeply $fl._parse( Q{~C} ), [ Format::Lisp::Directive::C.new ];
	is-deeply $fl._parse( Q{~^} ), [ Format::Lisp::Directive::Caret.new ];
	is-deeply $fl._parse( Q{~d} ), [ Format::Lisp::Directive::D.new ];
	is-deeply $fl._parse( Q{~D} ), [ Format::Lisp::Directive::D.new ];
	is-deeply $fl._parse( Q{~$} ), [ Format::Lisp::Directive::Dollar.new ];
	is-deeply $fl._parse( Q{~e} ), [ Format::Lisp::Directive::E.new ];
	is-deeply $fl._parse( Q{~E} ), [ Format::Lisp::Directive::E.new ];
	is-deeply $fl._parse( Q{~f} ), [ Format::Lisp::Directive::F.new ];
	is-deeply $fl._parse( Q{~F} ), [ Format::Lisp::Directive::F.new ];
	is-deeply $fl._parse( Q{~g} ), [ Format::Lisp::Directive::G.new ];
	is-deeply $fl._parse( Q{~G} ), [ Format::Lisp::Directive::G.new ];
	is-deeply $fl._parse( Q{~i} ), [ Format::Lisp::Directive::I.new ];
	is-deeply $fl._parse( Q{~I} ), [ Format::Lisp::Directive::I.new ];
	is-deeply $fl._parse( Q{~o} ), [ Format::Lisp::Directive::O.new ];
	is-deeply $fl._parse( Q{~O} ), [ Format::Lisp::Directive::O.new ];
	is-deeply $fl._parse( Q{~p} ), [ Format::Lisp::Directive::P.new ];
	is-deeply $fl._parse( Q{~P} ), [ Format::Lisp::Directive::P.new ];
	is-deeply $fl._parse( Q{~%} ), [ Format::Lisp::Directive::Percent.new ];
	is-deeply $fl._parse( Q{~|} ), [ Format::Lisp::Directive::Pipe.new ];
	is-deeply $fl._parse( Q{~?} ), [ Format::Lisp::Directive::Ques.new ];
	is-deeply $fl._parse( Q{~r} ), [ Format::Lisp::Directive::R.new ];
	is-deeply $fl._parse( Q{~R} ), [ Format::Lisp::Directive::R.new ];
	is-deeply $fl._parse( Q{~s} ), [ Format::Lisp::Directive::S.new ];
	is-deeply $fl._parse( Q{~S} ), [ Format::Lisp::Directive::S.new ];
	is-deeply $fl._parse( Q{~;} ), [ Format::Lisp::Directive::Semi.new ];
	is-deeply $fl._parse( Q{~/a/} ), [
		Format::Lisp::Directive::Slash.new(
			text => 'a'
		)
	];
	is-deeply $fl._parse( Q{~*} ), [ Format::Lisp::Directive::Star.new ];
	is-deeply $fl._parse( Q{~t} ), [ Format::Lisp::Directive::T.new ];
	is-deeply $fl._parse( Q{~T} ), [ Format::Lisp::Directive::T.new ];
	is-deeply $fl._parse( Q{~~} ), [ Format::Lisp::Directive::Tilde.new ];
	is-deeply $fl._parse( Q{~_} ), [ Format::Lisp::Directive::Under.new ];
	is-deeply $fl._parse( Q{~w} ), [ Format::Lisp::Directive::W.new ];
	is-deeply $fl._parse( Q{~W} ), [ Format::Lisp::Directive::W.new ];
	is-deeply $fl._parse( Q{~x} ), [ Format::Lisp::Directive::X.new ];
	is-deeply $fl._parse( Q{~X} ), [ Format::Lisp::Directive::X.new ];

	subtest {
		is-deeply $fl._parse( Q{~<~>} ), [
			Format::Lisp::Directive::Angle.new(
				children => [ ]
			)
		];
		is-deeply $fl._parse( Q{~{~}} ), [
			Format::Lisp::Directive::Brace.new(
				children => [ ]
			)
		];
		is-deeply $fl._parse( Q{~[~]} ), [
			Format::Lisp::Directive::Bracket.new(
				children => [ ]
			)
		];
		is-deeply $fl._parse( Q{~(~)} ), [
			Format::Lisp::Directive::Paren.new(
				children => [ ]
			)
		];
	}, 'balanced';

	done-testing;
}, 'single directive, no ornamentation';

subtest {
	subtest {
		is-deeply $fl._parse( Q{~<A~>} ), [
			Format::Lisp::Directive::Angle.new(
				children => [
					Format::Lisp::Text.new( text => 'A' )
				]
			)
		];
		is-deeply $fl._parse( Q{~{A~}} ), [
			Format::Lisp::Directive::Brace.new(
				children => [
					Format::Lisp::Text.new( text => 'A' )
				]
			)
		];
		is-deeply $fl._parse( Q{~[A~]} ), [
			Format::Lisp::Directive::Bracket.new(
				children => [
					Format::Lisp::Text.new( text => 'A' )
				]
			)
		];
		is-deeply $fl._parse( Q{~(A~)} ), [
			Format::Lisp::Directive::Paren.new(
				children => [
					Format::Lisp::Text.new( text => 'A' )
				]
			)
		];

		done-testing;

	}, 'nested directive with text content';

	subtest {
		is-deeply $fl._parse( Q{~<~A~>} ), [
			Format::Lisp::Directive::Angle.new(
				children => [
					Format::Lisp::Directive::A.new
				]
			)
		];
		is-deeply $fl._parse( Q{~{~A~}} ), [
			Format::Lisp::Directive::Brace.new(
				children => [
					Format::Lisp::Directive::A.new
				]
			)
		];
		is-deeply $fl._parse( Q{~[~A~]} ), [
			Format::Lisp::Directive::Bracket.new(
				children => [
					Format::Lisp::Directive::A.new
				]
			)
		];
		is-deeply $fl._parse( Q{~(~A~)} ), [
			Format::Lisp::Directive::Paren.new(
				children => [
					Format::Lisp::Directive::A.new
				]
			)
		];

		done-testing;

	}, 'nested directive with directive content';

	done-testing;

}, 'nested directives';

subtest {
	is-deeply $fl._parse( Q{~A~a} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~&} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Amp.new
	];
	is-deeply $fl._parse( Q{~A~b} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::B.new
	];
	is-deeply $fl._parse( Q{~A~B} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::B.new
	];
	is-deeply $fl._parse( Q{~A~c} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::C.new
	];
	is-deeply $fl._parse( Q{~A~C} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::C.new
	];
	is-deeply $fl._parse( Q{~A~^} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Caret.new
	];
	is-deeply $fl._parse( Q{~A~d} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::D.new
	];
	is-deeply $fl._parse( Q{~A~D} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::D.new
	];
	is-deeply $fl._parse( Q{~A~$} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Dollar.new
	];
	is-deeply $fl._parse( Q{~A~e} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::E.new
	];
	is-deeply $fl._parse( Q{~A~E} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::E.new
	];
	is-deeply $fl._parse( Q{~A~f} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::F.new
	];
	is-deeply $fl._parse( Q{~A~F} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::F.new
	];
	is-deeply $fl._parse( Q{~A~g} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::G.new
	];
	is-deeply $fl._parse( Q{~A~G} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::G.new
	];
	is-deeply $fl._parse( Q{~A~i} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::I.new
	];
	is-deeply $fl._parse( Q{~A~I} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::I.new
	];
	is-deeply $fl._parse( Q{~A~o} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::O.new
	];
	is-deeply $fl._parse( Q{~A~O} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::O.new
	];
	is-deeply $fl._parse( Q{~A~p} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::P.new
	];
	is-deeply $fl._parse( Q{~A~P} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::P.new
	];
	is-deeply $fl._parse( Q{~A~%} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Percent.new
	];
	is-deeply $fl._parse( Q{~A~|} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Pipe.new
	];
	is-deeply $fl._parse( Q{~A~?} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Ques.new
	];
	is-deeply $fl._parse( Q{~A~r} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::R.new
	];
	is-deeply $fl._parse( Q{~A~R} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::R.new
	];
	is-deeply $fl._parse( Q{~A~s} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::S.new
	];
	is-deeply $fl._parse( Q{~A~S} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::S.new
	];
	is-deeply $fl._parse( Q{~A~;} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Semi.new
	];
	is-deeply $fl._parse( Q{~A~/a/} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Slash.new(
			text => 'a'
		)
	];
	is-deeply $fl._parse( Q{~A~*} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new
	];
	is-deeply $fl._parse( Q{~A~t} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::T.new
	];
	is-deeply $fl._parse( Q{~A~T} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::T.new
	];
	is-deeply $fl._parse( Q{~A~~} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Tilde.new
	];
	is-deeply $fl._parse( Q{~A~_} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Under.new
	];
	is-deeply $fl._parse( Q{~A~w} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::W.new
	];
	is-deeply $fl._parse( Q{~A~W} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::W.new
	];
	is-deeply $fl._parse( Q{~A~x} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::X.new
	];
	is-deeply $fl._parse( Q{~A~X} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::X.new
	];

	done-testing;
}, 'paired directives';

subtest {
	is-deeply $fl._parse( Q{~A~<~>} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Angle.new( children => [ ] )
	];
	is-deeply $fl._parse( Q{~A~{~}} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Brace.new( children => [ ] )
	];
	is-deeply $fl._parse( Q{~A~[~]} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Bracket.new( children => [ ] )
	];
	is-deeply $fl._parse( Q{~A~(~)} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Paren.new( children => [ ] )
	];

	done-testing;
		
}, 'atom and nested directives';

subtest {
	is-deeply $fl._parse( Q{a~aa} ), [
		Format::Lisp::Text.new( text => 'a' ),
		Format::Lisp::Directive::A.new,
		Format::Lisp::Text.new( text => 'a' )
	];
	is-deeply $fl._parse( Q{A~AA} ), [
		Format::Lisp::Text.new( text => 'A' ),
		Format::Lisp::Directive::A.new,
		Format::Lisp::Text.new( text => 'A' )
	];
	is-deeply $fl._parse( Q{&~&&} ), [
		Format::Lisp::Text.new( text => '&' ),
		Format::Lisp::Directive::Amp.new,
		Format::Lisp::Text.new( text => '&' )
	];
	is-deeply $fl._parse( Q{<~<~>>} ), [
		Format::Lisp::Text.new( text => '<' ),
		Format::Lisp::Directive::Angle.new( children => [ ] ),
		Format::Lisp::Text.new( text => '>' )
	];
	is-deeply $fl._parse( Q{>~<~><} ), [
		Format::Lisp::Text.new( text => '>' ),
		Format::Lisp::Directive::Angle.new( children => [ ] ),
		Format::Lisp::Text.new( text => '<' )
	];
	is-deeply $fl._parse( Q{b~bb} ), [
		Format::Lisp::Text.new( text => 'b' ),
		Format::Lisp::Directive::B.new,
		Format::Lisp::Text.new( text => 'b' )
	];
	is-deeply $fl._parse( Q{B~BB} ), [
		Format::Lisp::Text.new( text => 'B' ),
		Format::Lisp::Directive::B.new,
		Format::Lisp::Text.new( text => 'B' )
	];
	is-deeply $fl._parse( Q[{~{~}}] ), [
		Format::Lisp::Text.new( text => '{' ),
		Format::Lisp::Directive::Brace.new( children => [ ] ),
		Format::Lisp::Text.new( text => '}' )
	];
	is-deeply $fl._parse( Q[}~{~}{] ), [
		Format::Lisp::Text.new( text => '}' ),
		Format::Lisp::Directive::Brace.new( children => [ ] ),
		Format::Lisp::Text.new( text => '{' )
	];
	is-deeply $fl._parse( Q{[~[~]]} ), [
		Format::Lisp::Text.new( text => '[' ),
		Format::Lisp::Directive::Bracket.new( children => [ ] ),
		Format::Lisp::Text.new( text => ']' )
	];
	is-deeply $fl._parse( Q{]~[~][} ), [
		Format::Lisp::Text.new( text => ']' ),
		Format::Lisp::Directive::Bracket.new( children => [ ] ),
		Format::Lisp::Text.new( text => '[' )
	];
	is-deeply $fl._parse( Q{c~cc} ), [
		Format::Lisp::Text.new( text => 'c' ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Text.new( text => 'c' )
	];
	is-deeply $fl._parse( Q{C~CC} ), [
		Format::Lisp::Text.new( text => 'C' ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Text.new( text => 'C' )
	];
	is-deeply $fl._parse( Q{^~^^} ), [
		Format::Lisp::Text.new( text => '^' ),
		Format::Lisp::Directive::Caret.new,
		Format::Lisp::Text.new( text => '^' )
	];
	is-deeply $fl._parse( Q{d~dd} ), [
		Format::Lisp::Text.new( text => 'd' ),
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'd' )
	];
	is-deeply $fl._parse( Q{D~DD} ), [
		Format::Lisp::Text.new( text => 'D' ),
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'D' )
	];
#`(
	is-deeply $fl._parse( Q{S~$$} ), [
		Format::Lisp::Text.new( text => Q{S} ),
		Format::Lisp::Directive::Dollar.new,
		Format::Lisp::Text.new( text => Q{S} )
	];
)
	is-deeply $fl._parse( Q{e~ee} ), [
		Format::Lisp::Text.new( text => 'e' ),
		Format::Lisp::Directive::E.new,
		Format::Lisp::Text.new( text => 'e' )
	];
	is-deeply $fl._parse( Q{E~EE} ), [
		Format::Lisp::Text.new( text => 'E' ),
		Format::Lisp::Directive::E.new,
		Format::Lisp::Text.new( text => 'E' )
	];
	is-deeply $fl._parse( Q{f~ff} ), [
		Format::Lisp::Text.new( text => 'f' ),
		Format::Lisp::Directive::F.new,
		Format::Lisp::Text.new( text => 'f' )
	];
	is-deeply $fl._parse( Q{F~FF} ), [
		Format::Lisp::Text.new( text => 'F' ),
		Format::Lisp::Directive::F.new,
		Format::Lisp::Text.new( text => 'F' )
	];
	is-deeply $fl._parse( Q{g~gg} ), [
		Format::Lisp::Text.new( text => 'g' ),
		Format::Lisp::Directive::G.new,
		Format::Lisp::Text.new( text => 'g' )
	];
	is-deeply $fl._parse( Q{G~GG} ), [
		Format::Lisp::Text.new( text => 'G' ),
		Format::Lisp::Directive::G.new,
		Format::Lisp::Text.new( text => 'G' )
	];
	is-deeply $fl._parse( Q{i~ii} ), [
		Format::Lisp::Text.new( text => 'i' ),
		Format::Lisp::Directive::I.new,
		Format::Lisp::Text.new( text => 'i' )
	];
	is-deeply $fl._parse( Q{I~II} ), [
		Format::Lisp::Text.new( text => 'I' ),
		Format::Lisp::Directive::I.new,
		Format::Lisp::Text.new( text => 'I' )
	];
	is-deeply $fl._parse( Q{o~oo} ), [
		Format::Lisp::Text.new( text => 'o' ),
		Format::Lisp::Directive::O.new,
		Format::Lisp::Text.new( text => 'o' )
	];
	is-deeply $fl._parse( Q{O~OO} ), [
		Format::Lisp::Text.new( text => 'O' ),
		Format::Lisp::Directive::O.new,
		Format::Lisp::Text.new( text => 'O' )
	];
	is-deeply $fl._parse( Q{p~pp} ), [
		Format::Lisp::Text.new( text => 'p' ),
		Format::Lisp::Directive::P.new,
		Format::Lisp::Text.new( text => 'p' )
	];
	is-deeply $fl._parse( Q{P~PP} ), [
		Format::Lisp::Text.new( text => 'P' ),
		Format::Lisp::Directive::P.new,
		Format::Lisp::Text.new( text => 'P' )
	];
	is-deeply $fl._parse( Q{(~(~))} ), [
		Format::Lisp::Text.new( text => '(' ),
		Format::Lisp::Directive::Paren.new( children => [ ] ),
		Format::Lisp::Text.new( text => ')' )
	];
	is-deeply $fl._parse( Q{)~(~)(} ), [
		Format::Lisp::Text.new( text => ')' ),
		Format::Lisp::Directive::Paren.new( children => [ ] ),
		Format::Lisp::Text.new( text => '(' )
	];
	is-deeply $fl._parse( Q{%~%%} ), [
		Format::Lisp::Text.new( text => '%' ),
		Format::Lisp::Directive::Percent.new,
		Format::Lisp::Text.new( text => '%' )
	];
	is-deeply $fl._parse( Q{|~||} ), [
		Format::Lisp::Text.new( text => '|' ),
		Format::Lisp::Directive::Pipe.new,
		Format::Lisp::Text.new( text => '|' )
	];
	is-deeply $fl._parse( Q{?~??} ), [
		Format::Lisp::Text.new( text => '?' ),
		Format::Lisp::Directive::Ques.new,
		Format::Lisp::Text.new( text => '?' )
	];
	is-deeply $fl._parse( Q{r~rr} ), [
		Format::Lisp::Text.new( text => 'r' ),
		Format::Lisp::Directive::R.new,
		Format::Lisp::Text.new( text => 'r' )
	];
	is-deeply $fl._parse( Q{R~RR} ), [
		Format::Lisp::Text.new( text => 'R' ),
		Format::Lisp::Directive::R.new,
		Format::Lisp::Text.new( text => 'R' )
	];
	is-deeply $fl._parse( Q{s~ss} ), [
		Format::Lisp::Text.new( text => 's' ),
		Format::Lisp::Directive::S.new,
		Format::Lisp::Text.new( text => 's' )
	];
	is-deeply $fl._parse( Q{S~SS} ), [
		Format::Lisp::Text.new( text => 'S' ),
		Format::Lisp::Directive::S.new,
		Format::Lisp::Text.new( text => 'S' )
	];
	is-deeply $fl._parse( Q{;~;;} ), [
		Format::Lisp::Text.new( text => ';' ),
		Format::Lisp::Directive::Semi.new,
		Format::Lisp::Text.new( text => ';' )
	];
	is-deeply $fl._parse( Q{/~/a//} ), [
		Format::Lisp::Text.new( text => '/' ),
		Format::Lisp::Directive::Slash.new(
			text => 'a'
		),
		Format::Lisp::Text.new( text => '/' )
	];
	is-deeply $fl._parse( Q{*~**} ), [
		Format::Lisp::Text.new( text => '*' ),
		Format::Lisp::Directive::Star.new,
		Format::Lisp::Text.new( text => '*' )
	];
	is-deeply $fl._parse( Q{t~tt} ), [
		Format::Lisp::Text.new( text => 't' ),
		Format::Lisp::Directive::T.new,
		Format::Lisp::Text.new( text => 't' )
	];
	is-deeply $fl._parse( Q{T~TT} ), [
		Format::Lisp::Text.new( text => 'T' ),
		Format::Lisp::Directive::T.new,
		Format::Lisp::Text.new( text => 'T' )
	];
#`(
	is-deeply $fl._parse( Q{\~~~} ), [
		Format::Lisp::Text.new( text => '~' ),
		Format::Lisp::Directive::Tilde.new
	];
)
	is-deeply $fl._parse( Q{_~__} ), [
		Format::Lisp::Text.new( text => '_' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => '_' )
	];
	is-deeply $fl._parse( Q{w~ww} ), [
		Format::Lisp::Text.new( text => 'w' ),
		Format::Lisp::Directive::W.new,
		Format::Lisp::Text.new( text => 'w' )
	];
	is-deeply $fl._parse( Q{W~WW} ), [
		Format::Lisp::Text.new( text => 'W' ),
		Format::Lisp::Directive::W.new,
		Format::Lisp::Text.new( text => 'W' )
	];
	is-deeply $fl._parse( Q{x~xx} ), [
		Format::Lisp::Text.new( text => 'x' ),
		Format::Lisp::Directive::X.new,
		Format::Lisp::Text.new( text => 'x' )
	];
	is-deeply $fl._parse( Q{X~XX} ), [
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::X.new,
		Format::Lisp::Text.new( text => 'X' )
	];

	done-testing;
		
}, 'Atom with text tricks';

subtest {
	is-deeply $fl._parse( Q{~:A} ), [
		Format::Lisp::Directive::A.new(
			colon => True
		)
	];
	is-deeply $fl._parse( Q{~@A} ), [
		Format::Lisp::Directive::A.new(
			at => True
		)
	];
	is-deeply $fl._parse( Q{~:@A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			colon => True
		)
	];
	is-deeply $fl._parse( Q{~@:A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			colon => True
		)
	];

	subtest {
		is-deeply $fl._parse( Q{~:<~>} ), [
			Format::Lisp::Directive::Angle.new(
				colon => True,
				children => [ ]
			)
		];
		is-deeply $fl._parse( Q{~@<~>} ), [
			Format::Lisp::Directive::Angle.new(
				at => True,
				children => [ ]
			)
		];
		is-deeply $fl._parse( Q{~:@<~>} ), [
			Format::Lisp::Directive::Angle.new(
				at => True,
				colon => True,
				children => [ ]
			)
		];
		is-deeply $fl._parse( Q{~@:<~>} ), [
			Format::Lisp::Directive::Angle.new(
				at => True,
				colon => True,
				children => [ ]
			)
		];

		done-testing;
		
	}, 'nested directive';

	done-testing;
		
}, 'options';

subtest {
	is-deeply $fl._parse( Q{~vA} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~VA} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~#A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ '#' ]
		)
	];
	is-deeply $fl._parse( Q{~-2A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ -2 ]
		)
	];
	is-deeply $fl._parse( Q{~-1A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ -1 ]
		)
	];
	is-deeply $fl._parse( Q{~-0A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 0 ]
		)
	];
	is-deeply $fl._parse( Q{~0A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 0 ]
		)
	];
	is-deeply $fl._parse( Q{~+0A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 0 ]
		)
	];
	is-deeply $fl._parse( Q{~+1A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 1 ]
		)
	];
	is-deeply $fl._parse( Q{~+2A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 2 ]
		)
	];
	is-deeply $fl._parse( Q{~'*A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ Q{'*} ]
		)
	];
	is-deeply $fl._parse( Q{~'AA} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ Q{'A} ]
		)
	];
	is-deeply $fl._parse( Q{~'aa} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ Q{'a} ]
		)
	];
#`(
	is-deeply $fl._parse( Q{~'#a} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ Q{'#} ]
		)
	];
)
	is-deeply $fl._parse( Q{~'va} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ Q{'v} ]
		)
	];
	is-deeply $fl._parse( Q{~'Va} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ Q{'V} ]
		)
	];
	is-deeply $fl._parse( Q{~',A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ Q{',} ]
		)
	];
#`(
	is-deeply $fl._parse( Q{~'\'A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ Q{''} ]
		)
	];
)

	done-testing;
		
}, 'single argument';

subtest {
	subtest {
		is-deeply $fl._parse( Q{~,A} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ Any, Any ]
			)
		];
		is-deeply $fl._parse( Q{~,vA} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ Any, 'V' ]
			)
		];
		is-deeply $fl._parse( Q{~,#A} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ Any, '#' ]
			)
		];
		is-deeply $fl._parse( Q{~,0A} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ Any, 0 ]
			)
		];

		done-testing;
	}, 'blank first argument';

	subtest {
		is-deeply $fl._parse( Q{~v,A} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ 'V', Any ]
			)
		];
		is-deeply $fl._parse( Q{~v,vA} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ 'V', 'V' ]
			)
		];
		is-deeply $fl._parse( Q{~v,#A} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ 'V', '#' ]
			)
		];
		is-deeply $fl._parse( Q{~v,0A} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ 'V', 0 ]
			)
		];

		done-testing;
	}, 'first argument "v"';

	subtest {
		is-deeply $fl._parse( Q{~#,A} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ '#', Any ]
			)
		];
		is-deeply $fl._parse( Q{~#,vA} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ '#', 'V' ]
			)
		];
		is-deeply $fl._parse( Q{~#,#A} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ '#', '#' ]
			)
		];
		is-deeply $fl._parse( Q{~#,0A} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ '#', 0 ]
			)
		];

		done-testing;
	}, 'first argument "#"';

	subtest {
		is-deeply $fl._parse( Q{~0,A} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ 0, Any ]
			)
		];
		is-deeply $fl._parse( Q{~0,vA} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ 0, 'V' ]
			)
		];
		is-deeply $fl._parse( Q{~0,#A} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ 0, '#' ]
			)
		];
		is-deeply $fl._parse( Q{~0,0A} ), [
			Format::Lisp::Directive::A.new(
				arguments => [ 0, 0 ]
			)
		];

		done-testing;
	}, 'first argument 0';

	done-testing;
		
}, 'multiple mixed arguments';

#`{
# Skip the $messages on all of these, as they get out of sync too easily.

subtest {
	is-deeply $fl._parse( Q{X~#%} ), [
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::Percent.new(
			arguments => [ '#' ]
		)
	];
	is-deeply $fl._parse( Q{X~V%} ), [
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::Percent.new(
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~@_A~%} ), [
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A' ),
		Format::Lisp::Directive::Percent.new
	];
	is-deeply $fl._parse( Q{~~~D%} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '%' )
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{X~%~&} ), [
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::Percent.new,
		Format::Lisp::Directive::Amp.new,
	];
	is-deeply $fl._parse( Q{X~V&} ), [
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::Amp.new(
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{X~~~D&} ), [
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '&' )
	];
	is-deeply $fl._parse( Q{~~~D%} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '%' )
	];

	done-testing;
}

subtest {
# Q{(~:@{~A~:^,~})},
# Q{(~:{~A~:^,~})},
	is-deeply $fl._parse( Q{(~A ~A)} ), [
		Format::Lisp::Text.new( text => '(' ),
		Format::Lisp::Directive::A.new,
		Format::Lisp::Text.new( text => ' ' ),
		Format::Lisp::Directive::A.new,
		Format::Lisp::Text.new( text => ')' )
	];
	is-deeply $fl._parse( Q{~(XXyy~AuuVV~)} ), [
		Format::Lisp::Directive::Paren.new(
			children => [
				Format::Lisp::Text.new( text => 'XXyy' ),
				Format::Lisp::Directive::A.new,
				Format::Lisp::Text.new( text => 'uuVV' )
			]
		)
	];
	is-deeply $fl._parse( Q{~(aBc ~(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~(aBc ~:(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					colon => True,
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~(aBc ~@(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					at => True,
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~@:(aBc ~(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			colon => True,
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~:(aBc ~:(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			colon => True,
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					colon => True,
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~:(aBc ~@(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			colon => True,
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					at => True,
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~:(aBc ~@:(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			colon => True,
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					at => True,
					colon => True,
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~:(this is a TEST.~)} ), [
		Format::Lisp::Directive::Paren.new(
			colon => True,
			children => [
				Format::Lisp::Text.new(
					text => 'this is a TEST.'
				)
			]
		)
	];
	is-deeply $fl._parse( Q{~:(this is7 a TEST.~)} ), [
		Format::Lisp::Directive::Paren.new(
			colon => True,
			children => [
				Format::Lisp::Text.new(
					text => 'this is7 a TEST.'
				)
			]
		)
	];
	is-deeply $fl._parse( Q{~:@(aBc ~(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			colon => True,
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~:@(aBc ~@(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			colon => True,
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					at => True,
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~:@(this is AlSo A teSt~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			colon => True,
			children => [
				Format::Lisp::Text.new(
					text => 'this is AlSo A teSt'
				)
			]
		)
	];
	is-deeply $fl._parse( Q{~@(!@#$%^&*this is a TEST.~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			children => [
				Format::Lisp::Text.new(
					text => '!@#$%^&*this is a TEST.'
				)
			]
		)
	];
	is-deeply $fl._parse( Q{~@(aBc ~(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~@(aBc ~:(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					colon => True,
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~@(aBc ~@(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					at => True,
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~@(aBc ~@:(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					at => True,
					colon => True,
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~@(this is a TEST.~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			children => [
				Format::Lisp::Text.new(
					text => 'this is a TEST.'
				)
			]
		)
	];
	is-deeply $fl._parse( Q{~@:(aBc ~:(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			colon => True,
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					colon => True,
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~@:(aBc ~@:(def~) GHi~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			colon => True,
			children => [
				Format::Lisp::Text.new( text => 'aBc ' ),
				Format::Lisp::Directive::Paren.new(
					at => True,
					colon => True,
					children => [
						Format::Lisp::Text.new(
							text => 'def'
						),
					]
				),
				Format::Lisp::Text.new( text => ' GHi' )
			]
		)
	];
	is-deeply $fl._parse( Q{~@:(~c~)} ), [
		Format::Lisp::Directive::Paren.new(
			at => True,
			colon => True,
			children => [
				Format::Lisp::Directive::C.new
			]
		)
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{'~c,} ), [
		Format::Lisp::Text.new( text => Q{'} ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Text.new( text => ',' )
	];
	is-deeply $fl._parse( Q{~~~d,} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ',' )
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~',@/cl-test::function-for-format-slash-19/} ), [
		Format::Lisp::Directive::Slash.new(
			at => True,
			arguments => [ Q{',} ],
			text => Q{cl-test::function-for-format-slash-19}
		)
	];
	is-deeply $fl._parse( Q{~'X:/cl-test::function-for-format-slash-19/} ), [
		Format::Lisp::Directive::Slash.new(
			colon => True,
			arguments => [ Q{'X} ],
			text => Q{cl-test::function-for-format-slash-19}
		)
	];
	is-deeply $fl._parse( Q{~-1@/cl-test::function-for-format-slash-19/} ), [
		Format::Lisp::Directive::Slash.new(
			at => True,
			arguments => [ -1 ],
			text => Q{cl-test::function-for-format-slash-19}
		)
	];
	is-deeply $fl._parse( Q{~/CL-TEST::FUNCTION-FOR-FORMAT-SLASH-9/} ), [
		Format::Lisp::Directive::Slash.new(
			text => Q{CL-TEST::FUNCTION-FOR-FORMAT-SLASH-9}
		)
	];
	is-deeply $fl._parse( Q{~/PPRINT-LINEAR/} ), [
		Format::Lisp::Directive::Slash.new( text => 'PPRINT-LINEAR' )
	];
	is-deeply $fl._parse( Q{~1,2,3,4,5,6,7,8,9,10@/cl-test::function-for-format-slash-19/} ), [
		Format::Lisp::Directive::Slash.new(
			text => Q{cl-test::function-for-format-slash-19},
			at => True,
			arguments => [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
		)
	];
	is-deeply $fl._parse( Q{~18@:/cl-test::function-for-format-slash-19/} ), [
		Format::Lisp::Directive::Slash.new(
			text => Q{cl-test::function-for-format-slash-19},
			at => True,
			colon => True,
			arguments => [ 18 ]
		)
	];
	is-deeply $fl._parse( Q{~:/cl-test::function-for-format-slash-19/} ), [
		Format::Lisp::Directive::Slash.new(
			text => Q{cl-test::function-for-format-slash-19},
			colon => True
		)
	];
	is-deeply $fl._parse( Q{~:/pprint-linear/} ), [
		Format::Lisp::Directive::Slash.new(
			text => Q{pprint-linear},
			colon => True
		)
	];
	is-deeply $fl._parse( Q{~:@/cl-test::function-for-format-slash-19/} ), [
		Format::Lisp::Directive::Slash.new(
			text => Q{cl-test::function-for-format-slash-19},
			colon => True,
			at => True
		)
	];
	is-deeply $fl._parse( Q{~@/cl-test::function-for-format-slash-19/} ), [
		Format::Lisp::Directive::Slash.new(
			text => Q{cl-test::function-for-format-slash-19},
			at => True
		)
	];
	is-deeply $fl._parse( Q{~@/pprint-linear/} ), [
		Format::Lisp::Directive::Slash.new(
			text => Q{pprint-linear},
			at => True
		)
	];
	is-deeply $fl._parse( Q{~@:/cl-test::function-for-format-slash-19/} ), [
		Format::Lisp::Directive::Slash.new(
			text => Q{cl-test::function-for-format-slash-19},
			colon => True,
			at => True
		)
	];
	is-deeply $fl._parse( Q{~@:/pprint-linear/} ), [
		Format::Lisp::Directive::Slash.new(
			text => Q{pprint-linear},
			at => True,
			colon => True
		)
	];
	is-deeply $fl._parse( Q{~v,v,v,v,v,v,v,v,v,v@/cl-test::function-for-format-slash-19/} ), [
		Format::Lisp::Directive::Slash.new(
			text => Q{cl-test::function-for-format-slash-19},
			at => True,
			arguments => [ 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v/cl-test::function-for-format-slash-19/} ), [
		Format::Lisp::Directive::Slash.new(
			text => Q{cl-test::function-for-format-slash-19},
			arguments => [ 'V' ]
		)
	];

	done-testing;
}

subtest {
# Q{XXX~<MMM~vI~:@_MMMMM~:>},
	is-deeply $fl._parse( Q{~%X ~,,1<~%X ~:;AAA~;BBB~;CCC~>} ), [
		Format::Lisp::Directive::Percent.new,
		Format::Lisp::Text.new( text => 'X ' ),
		Format::Lisp::Directive::Angle.new(
			arguments => [ Any, Any, 1 ],
			children => [
				Format::Lisp::Directive::Percent.new,
				Format::Lisp::Text.new( text => 'X ' ),
				Format::Lisp::Directive::Semi.new(
					colon => True
				),
				Format::Lisp::Text.new( text => 'AAA' ),
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Text.new( text => 'BBB' ),
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Text.new( text => 'CCC' )
			]
		)
	];
# Q{~%X ~<~%X ~0,30:;AAA~>~<~%X ~0,30:;BBB~>~<~%X ~0,30:;CCC~>},
# Q{~%X ~<~%X ~0,3:;AAA~>,~<~%X ~0,3:;BBB~>,~<~%X ~0,3:;CCC~>},
# Q{~%X ~<~%X ~0,3:;AAA~>~<~%X ~0,3:;BBB~>~<~%X ~0,3:;CCC~>},
	is-deeply $fl._parse( Q{~,,1,',<~A~;~A~>} ), [
		Format::Lisp::Directive::Angle.new(
			arguments => [ Any, Any, 1, Q{',} ],
			children => [
				Format::Lisp::Directive::A.new,
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Directive::A.new
			]
		)
	];
	is-deeply $fl._parse( Q{~,,1,v<~A~;~A~>} ), [
		Format::Lisp::Directive::Angle.new(
			arguments => [ Any, Any, 1, 'V' ],
			children => [
				Format::Lisp::Directive::A.new,
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Directive::A.new
			]
		)
	];
	is-deeply $fl._parse( Q{~,,1<~A~;~A~>} ), [
		Format::Lisp::Directive::Angle.new(
			arguments => [ Any, Any, 1 ],
			children => [
				Format::Lisp::Directive::A.new,
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Directive::A.new
			]
		)
	];
	is-deeply $fl._parse( Q{~,,2<~A~;~A~>} ), [
		Format::Lisp::Directive::Angle.new(
			arguments => [ Any, Any, 2 ],
			children => [
				Format::Lisp::Directive::A.new,
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Directive::A.new
			]
		)
	];
	is-deeply $fl._parse( Q{~,,v<~A~;~A~>} ), [
		Format::Lisp::Directive::Angle.new(
			arguments => [ Any, Any, 'V' ],
			children => [
				Format::Lisp::Directive::A.new,
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Directive::A.new
			]
		)
	];
	is-deeply $fl._parse( Q{~,v<~A~;~A~>} ), [
		Format::Lisp::Directive::Angle.new(
			arguments => [ Any, 'V' ],
			children => [
				Format::Lisp::Directive::A.new,
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Directive::A.new
			]
		)
	];
	is-deeply $fl._parse( Q{~,v:<~A~>} ), [
		Format::Lisp::Directive::Angle.new(
			colon => True,
			arguments => [ Any, 'V' ],
			children => [
				Format::Lisp::Directive::A.new
			]
		)
	];
	is-deeply $fl._parse( Q{~10:<abcdef~>} ), [
		Format::Lisp::Directive::Angle.new(
			colon => True,
			arguments => [ 10 ],
			children => [
				Format::Lisp::Text.new( text => 'abcdef' ),
			]
		)
	];
	is-deeply $fl._parse( Q{~10:@<abcdef~>} ), [
		Format::Lisp::Directive::Angle.new(
			at => True,
			colon => True,
			arguments => [ 10 ],
			children => [
				Format::Lisp::Text.new( text => 'abcdef' ),
			]
		)
	];
	is-deeply $fl._parse( Q{~10@<abcdef~>} ), [
		Format::Lisp::Directive::Angle.new(
			at => True,
			arguments => [ 10 ],
			children => [
				Format::Lisp::Text.new( text => 'abcdef' ),
			]
		)
	];
	is-deeply $fl._parse( Q{~13,,2<aaa~;bbb~;ccc~>} ), [
		Format::Lisp::Directive::Angle.new(
			arguments => [ 13, Any, 2 ],
			children => [
				Format::Lisp::Text.new( text => 'aaa' ),
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Text.new( text => 'bbb' ),
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Text.new( text => 'ccc' )
			]
		)
	];
	is-deeply $fl._parse( Q{~4@<~>} ), [
		Format::Lisp::Directive::Angle.new(
			at => True,
			arguments => [ 4 ],
			children => [ Any ]
		)
	];
	is-deeply $fl._parse( Q{~5:@<~>} ), [
		Format::Lisp::Directive::Angle.new(
			at => True,
			colon => True,
			arguments => [ 5 ],
			children => [ Any ]
		)
	];
	is-deeply $fl._parse( Q{~6:<~>} ), [
		Format::Lisp::Directive::Angle.new(
			colon => True,
			arguments => [ 6 ],
			children => [ Any ]
		)
	];
	is-deeply $fl._parse( Q{~6<abc~;def~^~>} ), [
		Format::Lisp::Directive::Angle.new(
			arguments => [ 6 ],
			children => [
				Format::Lisp::Text.new(
					text => 'abc'
				),
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Text.new(
					text => 'def'
				),
				Format::Lisp::Directive::Caret.new,
			]
		)
	];
	is-deeply $fl._parse( Q{~6@<abc~;def~^~>} ), [
		Format::Lisp::Directive::Angle.new(
			at => True,
			arguments => [ 6 ],
			children => [
				Format::Lisp::Text.new(
					text => 'abc'
				),
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Text.new(
					text => 'def'
				),
				Format::Lisp::Directive::Caret.new,
			]
		)
	];
# Q{~:<MMM~I~:@_MMMMM~:>},
# Q{~:<M~-1:i~:@_M~:>},
# Q{~:<M~-2:i~:@_M~:>},
# Q{~:<M~1:I~@:_M~:>},
# Q{~:<[~;~@{~A~^/~}~:>},
# Q{~:<[~;~@{~A~^/~}~;]~:>},
# Q{~:<~;~@{~A~^/~}~;]~:>},
# Q{~:<~@{~A~^ ~}~:>},
# Q{~:<~@{~A~^*~}~:>},
# Q{~:<~A~:>},
# Q{~:@<**~@;~@{~A~^       ~}~:@>},
# Q{~:@<~@{~A~^            ~:_~}~:>},
# Q{~:@<~@{~A~^            ~}~:@>},
# Q{~:@<~@{~A~^ ~_~}~:>},
# Q{~:@<~@{~A~^ ~}~:@>},
# Q{~:@<~@{~A~^~}~:@>},
# Q{~:@<~A~:>},
# Q{~<(~;M~-1:i~:@_M~;)~:>},
# Q{~<(~;M~:I~:@_M~;)~:>},
# Q{~<(~;M~v:i~:@_M~;)~:>},
# Q{~<ABC~;~v,0:T~;DEF~:>},
# Q{~<MMM~1I~:@_MMMMM~:>},
# Q{~<MMM~I~:@_MMMMM~:>},
# Q{~<M~3:i~:@_M~:>},
# Q{~<M~:i~:@_M~:>},
	is-deeply $fl._parse( Q{~<XXXXXX~;YYYYYYY~^~;ZZZZZ~>} ), [
		Format::Lisp::Directive::Angle.new(
			children => [
				Format::Lisp::Text.new(
					text => 'XXXXXX'
				),
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Text.new(
					text => 'YYYYYYY'
				),
				Format::Lisp::Directive::Caret.new,
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Text.new(
					text => 'ZZZZZ'
				),
			]
		)
	];
	is-deeply $fl._parse( Q{~<XXXXXX~;YYYYYYY~^~>} ), [
		Format::Lisp::Directive::Angle.new(
			children => [
				Format::Lisp::Text.new(
					text => 'XXXXXX'
				),
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Text.new(
					text => 'YYYYYYY'
				),
				Format::Lisp::Directive::Caret.new
			]
		)
	];
	is-deeply $fl._parse( Q{~<XXXXXX~^~>} ), [
		Format::Lisp::Directive::Angle.new(
			children => [
				Format::Lisp::Text.new(
					text => 'XXXXXX'
				),
				Format::Lisp::Directive::Caret.new
			]
		)
	];
# Q{~<XXXX~;~v,1:@t~:>},
# Q{~<XXX~;~,1:@t~;YYY~:>},
# Q{~<XXX~;~1,1:@t~;YYY~:>},
# Q{~<XXX~;~1,:@t~;YYY~:>},
# Q{~<XXX~;~1:@t~;YYY~:>},
# Q{~<XXX~;~:@t~;YYY~:>},
# Q{~<X~;~0,v:T~;Y~:>},
# Q{~<[~;XXXX~2,0:T~;]~:>},
# Q{~<[~;~,0:T~;]~:>},
# Q{~<[~;~0,0:T~;]~:>},
# Q{~<[~;~0,1:T~;]~:>},
# Q{~<[~;~0,:T~;]~:>},
# Q{~<[~;~0:T~;]~:>},
# Q{~<[~;~1,0:T~;]~:>},
# Q{~<[~;~2,0:T~;]~:>},
# Q{~<~/pprint-tabular/~:>},
# Q{~<~4:/pprint-tabular/~:>},
# Q{~<~:/pprint-tabular/~:>},
# Q{~<~:@/pprint-tabular/~:>},
# Q{~<~;~A~:>},
# Q{~<~;~A~;~:>},
	is-deeply $fl._parse( Q{~<~<XXXXXX~;YYYYYYY~^~>~>} ), [
		Format::Lisp::Directive::Angle.new(
			children => [
				Format::Lisp::Directive::Angle.new(
					children => [
						Format::Lisp::Text.new(
							text => 'XXXXXX'
						),
						Format::Lisp::Directive::Semi.new,
						Format::Lisp::Text.new(
							text => 'YYYYYYY'
						),
						Format::Lisp::Directive::Caret.new,
					]
				)
			]
		)
	];
# Q{~<~<~A~^xxx~:>yyy~:>},
	is-deeply $fl._parse( Q{~<~>} ), [
		Format::Lisp::Directive::Angle.new(
			children => [ Any ]
		)
	];
# Q{~<~@/pprint-tabular/~:>},
# Q{~<~@{~A~^*~}~:>},
# Q{~<~A~:>},
	is-deeply $fl._parse( Q{~<~A~;~A~>} ), [
		Format::Lisp::Directive::Angle.new(
			children => [
				Format::Lisp::Directive::A.new,
				Format::Lisp::Directive::Semi.new,
				Format::Lisp::Directive::A.new
			]
		)
	];
	is-deeply $fl._parse( Q{~<~A~>} ), [
		Format::Lisp::Directive::Angle.new(
			children => [
				Format::Lisp::Directive::A.new,
			]
		)
	];
# Q{~<~A~^xxxx~:>},
# Q{~<~v:/pprint-tabular/~:>},
# Q{~@:<~@{~A~^ ~:_~}~:>},
# Q{~@:<~@{~A~^ ~}~:>},
# Q{~@:<~A~:>},
# Q{~@<**~@;~@{~A~^       ~}~:@>},
# Q{~@<**~@;~@{~A~^       ~}~;XX~:@>},
# Q{~@<~;~A~:>},
# Q{~@<~;~A~;~:>},
# Q{~@<~@{~A~^ ~_~}~:>},
# Q{~@<~@{~A~^*~}~:>},
# Q{~@<~A~:>},
# Q{~A~<~A~v,v:t~:>},
# Q{~A~<~v,v:@t~:>},
# Q{~A~~<~A~~~D,~D:T~~:>}, # Actually not a <>
	is-deeply $fl._parse( Q{~v,,,v<~A~>} ), [
		Format::Lisp::Directive::Angle.new(
			arguments => [ 'V', Any, Any, 'V' ],
			children => [
				Format::Lisp::Directive::A.new,
			]
		)
	];
	is-deeply $fl._parse( Q{~v,,v<~A~>} ), [
		Format::Lisp::Directive::Angle.new(
			arguments => [ 'V', Any, 'V' ],
			children => [
				Format::Lisp::Directive::A.new,
			]
		)
	];
	is-deeply $fl._parse( Q{~v<~A~>} ), [
		Format::Lisp::Directive::Angle.new(
			arguments => [ 'V' ],
			children => [
				Format::Lisp::Directive::A.new,
			]
		)
	];
	is-deeply $fl._parse( Q{~~~d,,,'~c<~~A~~>} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => Q{,,,'} ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Text.new( text => '<' ),
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Text.new( text => 'A' ),
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Text.new( text => '>' ),
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~#,#@A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ '#', '#' ]
		)
	];
	is-deeply $fl._parse( Q{~#@A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ '#' ]
		)
	];
	is-deeply $fl._parse( Q{~#@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ '#' ]
		)
	];
	is-deeply $fl._parse( Q{~-100a} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ -100 ]
		),
	];
	is-deeply $fl._parse( Q{~-10,,,v@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ -10, Any, Any, 'V' ]
		),
	];
	is-deeply $fl._parse( Q{~-10,,,v@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ -10, Any, Any, 'V' ]
		),
	];
	is-deeply $fl._parse( Q{~-10,,,vA} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ -10, Any, Any, 'V' ]
		),
	];
	is-deeply $fl._parse( Q{~-10,,,va} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ -10, Any, Any, 'V' ]
		),
	];
	is-deeply $fl._parse( Q{~3,,+2a} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 3, Any, 2 ]
		),
	];
	is-deeply $fl._parse( Q{~3,,-1A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 3, Any, -1 ]
		),
	];
	is-deeply $fl._parse( Q{~3,,0A} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 3, Any, 0 ]
		),
	];
	is-deeply $fl._parse( Q{~3,,v@A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 3, Any, 'V' ]
		),
	];
	is-deeply $fl._parse( Q{~3,,vA} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 3, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~3,3@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 3, 3 ]
		)
	];
	is-deeply $fl._parse( Q{~4,#@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 4, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~4,,,'X@A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 4, Any, Any, Q{'X} ]
		)
	];
	is-deeply $fl._parse( Q{~4,,,'XA} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 4, Any, Any, Q{'X} ]
		)
	];
	is-deeply $fl._parse( Q{~4,,,@A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 4, Any, Any, Any ]
		)
	];
	is-deeply $fl._parse( Q{~4,,,a} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 4, Any, Any, Any ]
		)
	];
	is-deeply $fl._parse( Q{~4,,va} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 4, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~4,4@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 4, 4 ]
		)
	];
	is-deeply $fl._parse( Q{~5,#@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 5, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~5,3@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 5, 3 ]
		)
	];
	is-deeply $fl._parse( Q{~5,v@A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 5, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~7,3@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 7, 3 ]
		)
	];
	is-deeply $fl._parse( Q{~? ~A} ), [
		Format::Lisp::Directive::Ques.new,
		Format::Lisp::Text.new( text => ' ' ),
		Format::Lisp::Directive::A.new,
	];
	is-deeply $fl._parse( Q{~@? ~A} ), [
		Format::Lisp::Directive::Ques.new(
			at => True
		),
		Format::Lisp::Text.new( text => ' ' ),
		Format::Lisp::Directive::A.new,
	];
# Q{~@[X~]Y~A},
# Q{~@{~2,#^~A~}X~A},
	is-deeply $fl._parse( Q{~AY~?X~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Text.new( text => 'Y' ),
		Format::Lisp::Directive::Ques.new,
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::A.new,
	];
	is-deeply $fl._parse( Q{~AY~@?X~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Text.new( text => 'Y' ),
		Format::Lisp::Directive::Ques.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~*~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new,
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~0*~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			arguments => [ 0 ]
		),
		Format::Lisp::Directive::A.new
	];
# Q{~A~1{~A~*~A~}~A},
# Q{~A~1{~A~0*~A~}~A},
# Q{~A~1{~A~:*~A~}~A},
# Q{~A~1{~A~A~A~2:*~A~A~}~A},
# Q{~A~1{~A~A~A~:*~A~}~A},
# Q{~A~1{~A~A~v@*~A~A~}~A},
	is-deeply $fl._parse( Q{~A~:*~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			colon => True
		),
		Format::Lisp::Directive::A.new,
	];
	is-deeply $fl._parse( Q{~A~?X~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Ques.new,
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::A.new,
	];
	is-deeply $fl._parse( Q{~A~@?X~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Ques.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::A.new,
	];
	is-deeply $fl._parse( Q{~A~A~0:*~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			arguments => [ 0 ],
			colon => True
		),
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~A~1@*~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			arguments => [ 1 ],
			at => True
		),
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~A~2:*~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			arguments => [ 2 ],
			colon => True
		),
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~A~2@*~A~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			arguments => [ 2 ],
			at => True
		),
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~A~3@*~A~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			arguments => [ 3 ],
			at => True
		),
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~A~:*~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			colon => True
		),
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~A~@*~A~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			at => True
		),
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~A~v:*~A~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			colon => True,
			arguments => [ 'V' ]
		),
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~A~v@*~A~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			at => True,
			arguments => [ 'V' ]
		),
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new
	];
	is-deeply $fl._parse( Q{~A~v*~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			arguments => [ 'V' ]
		),
		Format::Lisp::Directive::A.new
	];
# Q{~A~{~A~*~A~}~A},
# Q{~A~{~A~A~0@*~A~A~}~A},
# Q{~A~{~A~A~1@*~A~}~A},
# Q{~A~{~A~A~@*~A~A~}~A},
# Q{~A~{~A~A~A~3:*~A~A~A~A~}~A},
# Q{~A~{~A~A~A~A~4:*~^~A~A~A~A~}~A},
# Q{~A~{~A~A~A~A~v*~^~A~A~A~A~}~A},
# Q{~A~{~A~A~A~A~v:*~^~A~}~A},
	is-deeply $fl._parse( Q{~V:@A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~V:@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~V:A} ), [
		Format::Lisp::Directive::A.new(
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~V:a} ), [
		Format::Lisp::Directive::A.new(
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~V@:A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~V@:a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~V@A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~V@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v,,2a} ), [
		Format::Lisp::Directive::A.new(
			arguments => [ 'V', Any, 2 ]
		)
	];
	is-deeply $fl._parse( Q{~v:@A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v:@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v:A} ), [
		Format::Lisp::Directive::A.new(
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v:a} ), [
		Format::Lisp::Directive::A.new(
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v@:A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v@:a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v@A} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v@a} ), [
		Format::Lisp::Directive::A.new(
			at => True,
			arguments => [ 'V' ]
		)
	];
# Q{~{~2,#^~A~}~A},
	is-deeply $fl._parse( Q{~~~d:a} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ':a' )
	];
	is-deeply $fl._parse( Q{~~~d@:a} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '@:a' ),
	];
	is-deeply $fl._parse( Q{~~~d@a} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '@a' ),
	];
	is-deeply $fl._parse( Q{~~~da} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'a' ),
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~~~db} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'b' )
	];
	is-deeply $fl._parse( Q{~~~d@b} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '@b' )
	];
	is-deeply $fl._parse( Q{~,,v:b} ), [
		Format::Lisp::Directive::B.new(
			colon => True,
			arguments => [ Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,V,V:b} ), [
		Format::Lisp::Directive::B.new(
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v,v:@b} ), [
		Format::Lisp::Directive::B.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#:b} ), [
		Format::Lisp::Directive::B.new(
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~db} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'b' ),
	];
	is-deeply $fl._parse( Q{~v,v,v,vb} ), [
		Format::Lisp::Directive::B.new(
			arguments => [ 'V', 'V', 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v:B} ), [
		Format::Lisp::Directive::B.new(
			colon => True,
			arguments => [ Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,V,V@:B} ), [
		Format::Lisp::Directive::B.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v,v:B} ), [
		Format::Lisp::Directive::B.new(
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,'*,v:B} ), [
		Format::Lisp::Directive::B.new(
			colon => True,
			arguments => [ Any, Any, Q{'*}, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#:B} ), [
		Format::Lisp::Directive::B.new(
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#@:B} ), [
		Format::Lisp::Directive::B.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~+10@B} ), [
		Format::Lisp::Directive::B.new(
			at => True,
			arguments => [ 10 ]
		),
	];
	is-deeply $fl._parse( Q{~V,V,V,VB} ), [
		Format::Lisp::Directive::B.new(
			arguments => [ 'V', 'V', 'V', 'V' ]
		)
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~~~d,'~c~c} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ',\'' ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Directive::C.new
	];
	is-deeply $fl._parse( Q{~~,,'~c:~c} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Text.new( text => Q{,,'} ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Text.new( text => ':' ),
		Format::Lisp::Directive::C.new
	];
	is-deeply $fl._parse( Q{'~c} ), [
		Format::Lisp::Text.new( text => Q{'} ),
		Format::Lisp::Directive::C.new
	];
	is-deeply $fl._parse( Q{#\\~:c} ), [
		Format::Lisp::Text.new( text => Q{#\\} ),
		Format::Lisp::Directive::C.new(
			colon => True
		)
	];
	is-deeply $fl._parse( Q{~~~d~c} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Directive::C.new,
	];
	is-deeply $fl._parse( Q{~~~d@~c} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '@' ),
		Format::Lisp::Directive::C.new
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~~~dd} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'd' )
	];
	is-deeply $fl._parse( Q{~~~d@d} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '@d' )
	];
	is-deeply $fl._parse( Q{~~~d,'~cd} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => Q{,'} ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Text.new( text => 'd' )
	];
	is-deeply $fl._parse( Q{~v,v@d} ), [
		Format::Lisp::Directive::D.new(
			at => True,
			arguments => [ 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v:d} ), [
		Format::Lisp::Directive::D.new(
			colon => True,
			arguments => [ Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~~,,'~c:d} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Text.new( text => Q{,,'} ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Text.new( text => ':d' )
	];
	is-deeply $fl._parse( Q{~,,v,v:d} ), [
		Format::Lisp::Directive::D.new(
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v,v:@d} ), [
		Format::Lisp::Directive::D.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,'*,v:d} ), [
		Format::Lisp::Directive::D.new(
			colon => True,
			arguments => [ Any, Any, Q{'*}, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#:d} ), [
		Format::Lisp::Directive::D.new(
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#:@d} ), [
		Format::Lisp::Directive::D.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~+10@d} ), [
		Format::Lisp::Directive::D.new(
			at => True,
			arguments => [ 10 ]
		),
	];
	is-deeply $fl._parse( Q{~dd} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'd' ),
	];
	is-deeply $fl._parse( Q{~v,v,v,vd} ), [
		Format::Lisp::Directive::D.new(
			arguments => [ 'V', 'V', 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#@:D} ), [
		Format::Lisp::Directive::D.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~v,v,v,vD} ), [
		Format::Lisp::Directive::D.new(
			arguments => [ 'V', 'V', 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v,v@D} ), [
		Format::Lisp::Directive::D.new(
			at => True,
			arguments => [ 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v,v:D} ), [
		Format::Lisp::Directive::D.new(
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v,v:@D} ), [
		Format::Lisp::Directive::D.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#:D} ), [
		Format::Lisp::Directive::D.new(
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~,,,,',f} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ Any, Any, Any, Any, Q{',} ]
		)
	];
	is-deeply $fl._parse( Q{~,,,,Vf} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ Any, Any, Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,,vf} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ Any, Any, Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,vF} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ Any, Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,2F} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ Any, Any, 2 ]
		)
	];
	is-deeply $fl._parse( Q{~,,Vf} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,vf} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,2f} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ Any, Any, 2 ]
		)
	];
	is-deeply $fl._parse( Q{~1,1,,f} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 1, 1, Any, Any ]
		)
	];
	is-deeply $fl._parse( Q{~10,1,,,'*F} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 10, 1, Any, Any, Q{'*} ]
		)
	];
	is-deeply $fl._parse( Q{~10,1,,,'*f} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 10, 1, Any, Any, Q{'*} ]
		)
	];
	is-deeply $fl._parse( Q{~10,1,,f} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 10, 1, Any, Any ]
		)
	];
	is-deeply $fl._parse( Q{~3@F} ), [
		Format::Lisp::Directive::F.new(
			at => True,
			arguments => [ 3 ]
		)
	];
	is-deeply $fl._parse( Q{~4,1,,'*f} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 4, 1, Any, Q{'*} ]
		)
	];
	is-deeply $fl._parse( Q{~4,1,-1F} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 4, 1, -1 ]
		)
	];
	is-deeply $fl._parse( Q{~4,2,-1F} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 4, 2, -1 ]
		)
	];
	is-deeply $fl._parse( Q{~4,2,0F} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 4, 2, 0 ]
		)
	];
	is-deeply $fl._parse( Q{~4,2,0f} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 4, 2, 0 ]
		)
	];
	is-deeply $fl._parse( Q{~4,2,1f} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 4, 2, 1 ]
		)
	];
	is-deeply $fl._parse( Q{~4,2@F} ), [
		Format::Lisp::Directive::F.new(
			at => True,
			arguments => [ 4, 2 ]
		)
	];
	is-deeply $fl._parse( Q{~4,2@f} ), [
		Format::Lisp::Directive::F.new(
			at => True,
			arguments => [ 4, 2 ]
		)
	];
	is-deeply $fl._parse( Q{~4@F} ), [
		Format::Lisp::Directive::F.new(
			at => True,
			arguments => [ 4 ]
		)
	];
	is-deeply $fl._parse( Q{~5,1,,'*F} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 5, 1, Any, Q{'*} ]
		)
	];
	is-deeply $fl._parse( Q{~5,1,,'*f} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 5, 1, Any, Q{'*} ]
		)
	];
	is-deeply $fl._parse( Q{~v,v,v,vf} ), [
		Format::Lisp::Directive::F.new(
			arguments => [ 'V', 'V', 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~~,,,,'~cf} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Text.new( text => ',,,,\'' ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Text.new( text => 'f' ),
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~+10@o} ), [
		Format::Lisp::Directive::O.new(
			at => True,
			arguments => [ 10 ]
		)
	];
	is-deeply $fl._parse( Q{~,,'*,v:@o} ), [
		Format::Lisp::Directive::O.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, Q{'*}, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#:@o} ), [
		Format::Lisp::Directive::O.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#:o} ), [
		Format::Lisp::Directive::O.new(
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#@:O} ), [
		Format::Lisp::Directive::O.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~,,V,v:@O} ), [
		Format::Lisp::Directive::O.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v,V:@O} ), [
		Format::Lisp::Directive::O.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v,v:@o} ), [
		Format::Lisp::Directive::O.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v,v:O} ), [
		Format::Lisp::Directive::O.new(
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v:o} ), [
		Format::Lisp::Directive::O.new(
			colon => True,
			arguments => [ Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v,V@O} ), [
		Format::Lisp::Directive::O.new(
			at => True,
			arguments => [ 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v,v,v,vo} ), [
		Format::Lisp::Directive::O.new(
			arguments => [ 'V', 'V', 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v,v@O} ), [
		Format::Lisp::Directive::O.new(
			at => True,
			arguments => [ 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~~~d@o} ), [
		Format::Lisp::Directive::Tilde.new( options => [] ),
		Format::Lisp::Directive::D.new( options => [] ),
		Format::Lisp::Text.new( text => '@o' ),
	];
	is-deeply $fl._parse( Q{~~~do} ), [
		Format::Lisp::Directive::Tilde.new( options => [] ),
		Format::Lisp::Directive::D.new( options => [] ),
		Format::Lisp::Text.new( text => 'o' ),
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~D cat~:p} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ' cat' ),
		Format::Lisp::Directive::P.new(
			colon => True
		)
	];
	is-deeply $fl._parse( Q{~D penn~:@p} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ' penn' ),
		Format::Lisp::Directive::P.new(
			at => True,
			colon => True
		)
	];
	is-deeply $fl._parse( Q{~D penn~@:p} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ' penn' ),
		Format::Lisp::Directive::P.new(
			at => True,
			colon => True
		)
	];
	is-deeply $fl._parse( Q{~D cat~:P} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ' cat' ),
		Format::Lisp::Directive::P.new(
			colon => True
		)
	];
	is-deeply $fl._parse( Q{~D penn~:@P} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ' penn' ),
		Format::Lisp::Directive::P.new(
			at => True,
			colon => True
		)
	];
	is-deeply $fl._parse( Q{~D penn~@:P} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ' penn' ),
		Format::Lisp::Directive::P.new(
			at => True,
			colon => True
		)
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~10,,,vr} ), [
		Format::Lisp::Directive::R.new(
			arguments => [ 10, Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~10,-100000000000000000000r} ), [
		Format::Lisp::Directive::R.new(
			arguments => [ 10, -100000000000000000000 ]
		),
	];
	is-deeply $fl._parse( Q{~16,,,,#:r} ), [
		Format::Lisp::Directive::R.new(
			colon => True,
			arguments => [ 16, Any, Any, Any, '#' ]
		),
	];
	is-deeply $fl._parse( Q{~2,,,,-100000000000000000000r} ), [
		Format::Lisp::Directive::R.new(
			arguments => [ 2, Any, Any, Any, -100000000000000000000 ]
		),
	];
	is-deeply $fl._parse( Q{~2,12,,'*:r} ), [
		Format::Lisp::Directive::R.new(
			colon => True,
			arguments => [ 2, 12, Any, Q{'*} ]
		)
	];
	is-deeply $fl._parse( Q{~2:r} ), [
		Format::Lisp::Directive::R.new(
			colon => True,
			arguments => [ 2 ]
		)
	];
	is-deeply $fl._parse( Q{~3@:r} ), [
		Format::Lisp::Directive::R.new(
			at => True,
			colon => True,
			arguments => [ 3 ]
		)
	];
	is-deeply $fl._parse( Q{~8,10:@r} ), [
		Format::Lisp::Directive::R.new(
			at => True,
			colon => True,
			arguments => [ 8, 10 ]
		)
	];
	is-deeply $fl._parse( Q{~dr} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'r' )
	];
	is-deeply $fl._parse( Q{~v,v,v,v,vr} ), [
		Format::Lisp::Directive::R.new(
			arguments => [ 'V', 'V', 'V', 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~~~D,~D,'*r} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ',' ),
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => Q{,'*r} ),
	];
	is-deeply $fl._parse( Q{~3,14,'X,',:R} ), [
		Format::Lisp::Directive::R.new(
			colon => True,
			arguments => [ 3, 14, Q{'X}, Q{',} ]
		)
	];
	is-deeply $fl._parse( Q{~8,,,,v:R} ), [
		Format::Lisp::Directive::R.new(
			colon => True,
			arguments => [ 8, Any, Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~8@R} ), [
		Format::Lisp::Directive::R.new(
			at => True,
			arguments => [ 8 ]
		)
	];
	is-deeply $fl._parse( Q{~~~D,~DR} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ',' ),
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'R' )
	];
	is-deeply $fl._parse( Q{~~~d,,,'~c,~d:R} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => Q{,,,'} ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Text.new( text => ',' ),
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ':R' )
	];
	is-deeply $fl._parse( Q{~~~d:R} ), [
		Format::Lisp::Directive::Tilde.new( options => [] ),
		Format::Lisp::Directive::D.new( options => [] ),
		Format::Lisp::Text.new( text => ':R' ),
	];
	is-deeply $fl._parse( Q{~~~dR} ), [
		Format::Lisp::Directive::Tilde.new( options => [] ),
		Format::Lisp::Directive::D.new( options => [] ),
		Format::Lisp::Text.new( text => 'R' ),
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~10,,,v@s} ), [
		Format::Lisp::Directive::S.new(
			at => True,
			arguments => [ 10, Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~10,,,vs} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 10, Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~3,,vs} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 3, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~3,3@s} ), [
		Format::Lisp::Directive::S.new(
			at => True,
			arguments => [ 3, 3 ]
		)
	];
	is-deeply $fl._parse( Q{~4,,,'X@s} ), [
		Format::Lisp::Directive::S.new(
			at => True,
			arguments => [ 4, Any, Any, Q{'X} ]
		)
	];
	is-deeply $fl._parse( Q{~4,,,s} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 4, Any, Any, Any ]
		)
	];
	is-deeply $fl._parse( Q{~4,,vs} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 4, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~4,4@s} ), [
		Format::Lisp::Directive::S.new(
			at => True,
			arguments => [ 4, 4 ]
		)
	];
	is-deeply $fl._parse( Q{~5,3@s} ), [
		Format::Lisp::Directive::S.new(
			at => True,
			arguments => [ 5, 3 ]
		)
	];
	is-deeply $fl._parse( Q{~7,3@s} ), [
		Format::Lisp::Directive::S.new(
			at => True,
			arguments => [ 7, 3 ]
		)
	];
	is-deeply $fl._parse( Q{~V,,2@s} ), [
		Format::Lisp::Directive::S.new(
			at => True,
			arguments => [ 'V', Any, 2 ]
		)
	];
	is-deeply $fl._parse( Q{~V:s} ), [
		Format::Lisp::Directive::S.new(
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~V@:s} ), [
		Format::Lisp::Directive::S.new(
			at => True,
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v:@s} ), [
		Format::Lisp::Directive::S.new(
			at => True,
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~~~d:s} ), [
		Format::Lisp::Directive::Tilde.new( options => [] ),
		Format::Lisp::Directive::D.new( options => [] ),
		Format::Lisp::Text.new( text => ':s' ),
	];
	is-deeply $fl._parse( Q{~~~d@s} ), [
		Format::Lisp::Directive::Tilde.new( options => [] ),
		Format::Lisp::Directive::D.new( options => [] ),
		Format::Lisp::Text.new( text => '@s' ),
	];
	is-deeply $fl._parse( Q{~10,,,v@S} ), [
		Format::Lisp::Directive::S.new(
			at => True,
			arguments => [ 10, Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~10,,,vS} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 10, Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~3,,2S} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 3, Any, 2 ]
		)
	];
	is-deeply $fl._parse( Q{~3,,-1S} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 3, Any, -1 ]
		)
	];
	is-deeply $fl._parse( Q{~3,,0S} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 3, Any, 0 ]
		)
	];
	is-deeply $fl._parse( Q{~3,,VS} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 3, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~3,,vS} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 3, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~4,,,'XS} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 4, Any, Any, Q{'X} ]
		)
	];
	is-deeply $fl._parse( Q{~4,,,@S} ), [
		Format::Lisp::Directive::S.new(
			at => True,
			arguments => [ 4, Any, Any, Any ]
		)
	];
	is-deeply $fl._parse( Q{~5,v@S} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 5, 'V' ],
			at => True
		)
	];
	is-deeply $fl._parse( Q{~v,,2S} ), [
		Format::Lisp::Directive::S.new(
			arguments => [ 'V', Any, 2 ]
		)
	];
	is-deeply $fl._parse( Q{~v:S} ), [
		Format::Lisp::Directive::S.new(
			colon => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v@S} ), [
		Format::Lisp::Directive::S.new(
			at => True,
			arguments => [ 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~~~d@:S} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '@:S' ),
	];
	is-deeply $fl._parse( Q{~~~dS} ), [
		Format::Lisp::Directive::Tilde.new( options => [] ),
		Format::Lisp::Directive::D.new( options => [] ),
		Format::Lisp::Text.new( text => 'S' ),
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~1,1@t} ), [
		Format::Lisp::Directive::T.new(
			at => True,
			arguments => [ 1, 1 ]
		)
	];
	is-deeply $fl._parse( Q{~A~v,vt} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::T.new(
			arguments => [ 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v,1@t~0,v@T} ), [
		Format::Lisp::Directive::T.new(
			at => True,
			arguments => [ 'V', 1 ]
		),
		Format::Lisp::Directive::T.new(
			at => True,
			arguments => [ 0, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v,v@t} ), [
		Format::Lisp::Directive::T.new(
			at => True,
			arguments => [ 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~~~d,~d@t} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ',' ),
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '@t' )
	];
	is-deeply $fl._parse( Q{XXXXX~2,0T} ), [
		Format::Lisp::Text.new( text => 'XXXXX' ),
		Format::Lisp::Directive::T.new(
			arguments => [ 2, 0 ]
		)
	];
	is-deeply $fl._parse( Q{~A~~~D,~DT} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ',' ),
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'T' )
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~,,'*,v:x} ), [
		Format::Lisp::Directive::X.new(
			colon => True,
			arguments => [ Any, Any, Q{'*}, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#:x} ), [
		Format::Lisp::Directive::X.new(
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~,,V:x} ), [
		Format::Lisp::Directive::X.new(
			colon => True,
			arguments => [ Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v,V:@x} ), [
		Format::Lisp::Directive::X.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v,v:@x} ), [
		Format::Lisp::Directive::X.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v:x} ), [
		Format::Lisp::Directive::X.new(
			colon => True,
			arguments => [ Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~dx} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'x' )
	];
	is-deeply $fl._parse( Q{~v,v,v,vx} ), [
		Format::Lisp::Directive::X.new(
			arguments => [ 'V', 'V', 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v,v@x} ), [
		Format::Lisp::Directive::X.new(
			at => True,
			arguments => [ 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~+10@X} ), [
		Format::Lisp::Directive::X.new(
			at => True,
			arguments => [ 10 ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#:X} ), [
		Format::Lisp::Directive::X.new(
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~,,,#@:X} ), [
		Format::Lisp::Directive::X.new(
			at => True,
			colon => True,
			arguments => [ Any, Any, Any, '#' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v,v:X} ), [
		Format::Lisp::Directive::X.new(
			colon => True,
			arguments => [ Any, Any, 'V', 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~,,v:X} ), [
		Format::Lisp::Directive::X.new(
			colon => True,
			arguments => [ Any, Any, 'V' ]
		)
	];
	is-deeply $fl._parse( Q{~v,V@X} ), [
		Format::Lisp::Directive::X.new(
			at => True,
			arguments => [ 'V', 'V' ]
		)
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{XX~10,20:@tYY} ), [
		Format::Lisp::Text.new( text => 'XX' ),
		Format::Lisp::Directive::T.new(
			at => True,
			colon => True,
			arguments => [ 10,20 ]
		),
		Format::Lisp::Text.new( text => 'YY' ),
	];
	is-deeply $fl._parse( Q{XX~10,20@:tYY} ), [
		Format::Lisp::Text.new( text => 'XX' ),
		Format::Lisp::Directive::T.new(
			at => True,
			colon => True,
			arguments => [ 10,20 ]
		),
		Format::Lisp::Text.new( text => 'YY' ),
	];
	is-deeply $fl._parse( Q{XX~10:tYY} ), [
		Format::Lisp::Text.new( text => 'XX' ),
		Format::Lisp::Directive::T.new(
			colon => True,
			arguments => [ 10 ]
		),
		Format::Lisp::Text.new( text => 'YY' ),
	];
	is-deeply $fl._parse( Q{X~AY} ), [
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::A.new,
		Format::Lisp::Text.new( text => 'Y' ),
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{a~?z} ), [
		Format::Lisp::Text.new( text => 'a' ),
		Format::Lisp::Directive::Ques.new,
		Format::Lisp::Text.new( text => 'z' ),
	];
	is-deeply $fl._parse( Q{a~@?z} ), [
		Format::Lisp::Text.new( text => 'a' ),
		Format::Lisp::Directive::Ques.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'z' ),
	];

	done-testing;
}

subtest {
# Q{~#[A~:;B~]},
# Q{~#[A~;B~]},
# Q{~-1[a~;b~;c~;d~]},
# Q{~0[a~;b~;c~;d~]},
# Q{~100000000000000000000000000000000[a~;b~;c~;d~]},
# Q{~1[a~;b~;c~;d~]},
# Q{~4[a~;b~;c~;d~]},
# Q{~:[a~;b~]},
# Q{~V[a~;b~;c~;d~;e~;f~;g~;h~;i~]},
# Q{~[a~:;b~]},
# Q{~[a~;b~;c~;d~:;e~]},
# Q{~[a~;b~;c~;d~;e~;f~;g~;h~;i~]},
# Q{~[a~]},
# Q{~[~:;a~]},
# Q{~[~]},
# Q{~v[a~;b~;c~;d~:;e~]},
# Q{~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]},

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{A             ~_} ), [
		Format::Lisp::Text.new( text => 'A             ' ),
		Format::Lisp::Directive::Under.new
	];
	is-deeply $fl._parse( Q{A ~:@_A ~:@_A ~:@_A ~:@_} ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		)
	];
	is-deeply $fl._parse( Q{A ~:@_A ~:@_A ~:@_A ~:@_A ~:@_} ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		)
	];
	is-deeply $fl._parse( Q{A ~:_A ~:_A ~:_A ~:_A ~:_} ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		)
	];
	is-deeply $fl._parse( Q{A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_} ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			colon => True
		)
	];
	is-deeply $fl._parse( Q{A ~@:_A ~@:_A ~@:_A ~@:_} ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		),
	];
	is-deeply $fl._parse( Q{A ~@_A ~@_A ~@_A ~@_} ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
	];
	is-deeply $fl._parse( Q{A ~@_A ~@_A ~@_A ~@_A ~@_} ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
	];
	is-deeply $fl._parse( Q{A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_} ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True
		)
	];
	is-deeply $fl._parse( Q{A ~@:_A } ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		),
		Format::Lisp::Text.new( text => 'A ' )
	];
	is-deeply $fl._parse( Q{A ~_A ~_A ~_A ~_} ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
	];
	is-deeply $fl._parse( Q{A ~_A ~_A ~_A ~_A ~_} ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
	];
	is-deeply $fl._parse( Q{A ~_A ~_A ~_A ~_A ~_A ~_A ~_A ~_A ~_A ~_} ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
	];
	is-deeply $fl._parse( Q{A ~_A ~_A ~_A ~_~%A ~_A ~_A ~_A ~_} ), [
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Directive::Percent.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
		Format::Lisp::Text.new( text => 'A ' ),
		Format::Lisp::Directive::Under.new,
	];
	is-deeply $fl._parse( Q{AAAA ~:@_} ), [
		Format::Lisp::Text.new( text => 'AAAA ' ),
		Format::Lisp::Directive::Under.new(
			at => True,
			colon => True
		)
	];
	is-deeply $fl._parse( Q{AAAA ~_} ), [
		Format::Lisp::Text.new( text => 'AAAA ' ),
		Format::Lisp::Directive::Under.new,
	];
	is-deeply $fl._parse( Q{B ~_} ), [
		Format::Lisp::Text.new( text => 'B ' ),
		Format::Lisp::Directive::Under.new,
	];
	is-deeply $fl._parse( Q{D ~_} ), [
		Format::Lisp::Text.new( text => 'D ' ),
		Format::Lisp::Directive::Under.new,
	];
	is-deeply $fl._parse( Q{~%A~@_} ), [
		Format::Lisp::Directive::Percent.new,
		Format::Lisp::Text.new( text => 'A' ),
		Format::Lisp::Directive::Under.new(
			at => True
		)
	];
	is-deeply $fl._parse( Q{~W~W~:_~W~W~:_~W~W~:_~W~W~:_~W~W~:_} ), [
		Format::Lisp::Directive::W.new,
		Format::Lisp::Directive::W.new,
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Directive::W.new,
		Format::Lisp::Directive::W.new,
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Directive::W.new,
		Format::Lisp::Directive::W.new,
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Directive::W.new,
		Format::Lisp::Directive::W.new,
		Format::Lisp::Directive::Under.new(
			colon => True
		),
		Format::Lisp::Directive::W.new,
		Format::Lisp::Directive::W.new,
		Format::Lisp::Directive::Under.new(
			colon => True
		),
	];

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~~~D|} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '|' )
	];

	done-testing;
}

subtest {
# Q{~#:@{A~:}},
# Q{~#:{~A~}},
# Q{~#@{~A~}},
# Q{~#{~A~}},
# Q{~#{~}},
# Q{~0:@{~A~:}},
# Q{~0:{XYZ~}},
# Q{~0@{~A~^~A~}},
# Q{~0{FOO~:}},
# Q{~0{~A~^~A~}},
# Q{~0{~}},
# Q{~1@{FOO~}},
# Q{~1@{~A~^~A~}},
# Q{~1{FOO~:}},
# Q{~1{~A~^~A~}},
# Q{~1{~}},
# Q{~2:{XYZ~}},
# Q{~2:{~A~}},
# Q{~2{FOO~:}},
# Q{~2{FOO~}},
# Q{~3{~}},
# Q{~:@{(~A ~A)~}},
# Q{~:@{~#,#,#:^~A~}},
# Q{~:@{~#,#,#^~A~}},
# Q{~:@{~#,#,2:^~A~}},
# Q{~:@{~#,#,3^~A~}},
# Q{~:@{~#,#:^~A~}},
# Q{~:@{~#,#^~A~}},
# Q{~:@{~#,1:^~A~}},
# Q{~:@{~#,1^~A~}},
# Q{~:@{~#,2,2:^~A~}},
# Q{~:@{~#,3,3^~A~}},
# Q{~:@{~#,v:^~A~}},
# Q{~:@{~#:^~A~}},
# Q{~:@{~#^~A~}},
# Q{~:@{~'X,'Y:^~A~}},
# Q{~:@{~'X:^~A~}},
# Q{~:@{~'x,'x^~A~}},
# Q{~:@{~'x,3^~A~}},
# Q{~:@{~0,1:^~A~}},
# Q{~:@{~0,3,#^~A~}},
# Q{~:@{~0,v^~A~}},
# Q{~:@{~0:^~A~}},
# Q{~:@{~1,#:^~A~}},
# Q{~:@{~1,#^~A~}},
# Q{~:@{~1,1,1^~A~}},
# Q{~:@{~1,1,v:^~A~}},
# Q{~:@{~1,1:^~A~}},
# Q{~:@{~1,2,1:^~A~}},
# Q{~:@{~1,2,1^~A~}},
# Q{~:@{~1,V:^~A~}},
# Q{~:@{~1,v,2:^~A~}},
# Q{~:@{~1,v,3^~A~}},
# Q{~:@{~1,v,v^~A~}},
# Q{~:@{~1,v^~A~}},
# Q{~:@{~1:^~A~}},
# Q{~:@{~2,#,3:^~A~}},
# Q{~:@{~2,#,3^~A~}},
# Q{~:@{~2,1,3:^~A~}},
# Q{~:@{~2,V,v:^~A~}},
# Q{~:@{~2,v^~A~}},
# Q{~:@{~3,#,#:^~A~}},
# Q{~:@{~3,#,#^~A~}},
# Q{~:@{~3,'x^~A~}},
# Q{~:@{~3,2,1^~A~}},
# Q{~:@{~:^~A~}},
# Q{~:@{~A~:}},
# Q{~:@{~A~^~A~A~}},
# Q{~:@{~V,#:^~A~}},
# Q{~:@{~V,v,3:^~A~}},
# Q{~:@{~V,v:^~A~}},
# Q{~:@{~V:^~A~}},
# Q{~:@{~v,1,v^~A~}},
# Q{~:@{~v,1:^~A~}},
# Q{~:@{~v,2,2:^~A~}},
# Q{~:@{~v,2,3^~A~}},
# Q{~:@{~v,2,v:^~A~}},
# Q{~:@{~v,3^~A~}},
# Q{~:@{~v,3^~A~}},
# Q{~:@{~v,v,V:^~A~}},
# Q{~:@{~v,v^~A~}},
# Q{~:@{~v,v^~A~}},
# Q{~:@{~v:^~A~}},
# Q{~:@{~v^~A~}},
# Q{~:{(~A ~A)~}},
# Q{~:{ABC~:}},
# Q{~:{~#,#,#:^~A~}},
# Q{~:{~#,#,#^~A~}},
# Q{~:{~#,#,2:^~A~}},
# Q{~:{~#,#,3^~A~}},
# Q{~:{~#,#:^~A~}},
# Q{~:{~#,#^~A~}},
# Q{~:{~#,1:^~A~}},
# Q{~:{~#,1^~A~}},
# Q{~:{~#,2,#:^~A~}},
# Q{~:{~#,2,2:^~A~}},
# Q{~:{~#,3,#^~A~}},
# Q{~:{~#,3,3^~A~}},
# Q{~:{~#,v:^~A~}},
# Q{~:{~#:^~A~}},
# Q{~:{~#^~A~#^~A~#^~A~#^~A~}},
# Q{~:{~#^~A~}},
# Q{~:{~'X,'X:^~A~}},
# Q{~:{~'X,'Y:^~A~}},
# Q{~:{~'X:^~A~}},
# Q{~:{~'x,'x^~A~}},
# Q{~:{~'x,3^~A~}},
# Q{~:{~0,1:^~A~}},
# Q{~:{~0,3,#^~A~}},
# Q{~:{~0,v^~A~}},
# Q{~:{~0:^~A~}},
# Q{~:{~1,#:^~A~}},
# Q{~:{~1,#^~A~}},
# Q{~:{~1,0,1^~A~}},
# Q{~:{~1,1,1^~A~}},
# Q{~:{~1,1,v:^~A~}},
# Q{~:{~1,1:^~A~}},
# Q{~:{~1,2,1:^~A~}},
# Q{~:{~1,2,1^~A~}},
# Q{~:{~1,2,3:^~A~}},
# Q{~:{~1,2,3^~A~}},
# Q{~:{~1,2,v^~A~}},
# Q{~:{~1,2,v^~A~}},
# Q{~:{~1,3,#:^~A~}},
# Q{~:{~1,V:^~A~}},
# Q{~:{~1,v,2:^~A~}},
# Q{~:{~1,v,3^~A~}},
# Q{~:{~1,v,v^~A~}},
# Q{~:{~1,v^~A~}},
# Q{~:{~1:^~A~}},
# Q{~:{~2,#,3:^~A~}},
# Q{~:{~2,#,3^~A~}},
# Q{~:{~2,1,3:^~A~}},
# Q{~:{~2,V,v:^~A~}},
# Q{~:{~2,v^~A~}},
# Q{~:{~3,#,#:^~A~}},
# Q{~:{~3,#,#^~A~}},
# Q{~:{~3,'x^~A~}},
# Q{~:{~3,2,1^~A~}},
# Q{~:{~3,v^~A~}},
# Q{~:{~:^~A~}},
# Q{~:{~A~0^~A~A~}},
# Q{~:{~A~:}},
# Q{~:{~A~^~A~A~}},
# Q{~:{~V,#:^~A~}},
# Q{~:{~V,v,3:^~A~}},
# Q{~:{~V,v:^~A~}},
# Q{~:{~V:^~A~}},
# Q{~:{~v,1,v^~A~}},
# Q{~:{~v,1:^~A~}},
# Q{~:{~v,2,2:^~A~}},
# Q{~:{~v,2,3^~A~}},
# Q{~:{~v,2,v:^~A~}},
# Q{~:{~v,3^~A~}},
# Q{~:{~v,3^~A~}},
# Q{~:{~v,v,V:^~A~}},
# Q{~:{~v,v^~A~}},
# Q{~:{~v:^~A~}},
# Q{~:{~v^~A~}},
# Q{~@:{~#^~A~#^~A~#^~A~#^~A~}},
# Q{~@:{~3,v^~A~}},
# Q{~@:{~A~0^~A~A~}},
# Q{~@{X ~A Y Z~}},
# Q{~@{X ~A~^ Y ~A~^ ~}},
# Q{~@{X~:}},
# Q{~@{~#,#,#^~A~}},
# Q{~@{~#,#,v^~A~}},
# Q{~@{~#,#^~A~}},
# Q{~@{~#,1,2^~A~}},
# Q{~@{~#,3^~A~}},
# Q{~@{~',,',^~A~}},
# Q{~@{~'X,v^~A~}},
# Q{~@{~'X^~A~}},
# Q{~@{~0,v,v^~A~}},
# Q{~@{~0,v^~A~}},
# Q{~@{~1,1,v^~A~}},
# Q{~@{~1,2,v^~A~}},
# Q{~@{~1,v,v^~A~}},
# Q{~@{~1,v^~A~}},
# Q{~@{~1{~A~}~}},
# Q{~@{~A~A~0^~A~}},
# Q{~@{~A~A~v^~A~}},
# Q{~@{~v,'X^~A~}},
# Q{~@{~v,1,v^~A~}},
# Q{~@{~v,v,v^~A~}},
# Q{~@{~v,v^~A~}},
# Q{~@{~v,v^~A~}},
# Q{~@{~{~A~}~}},
# Q{~V:@{~A~}},
# Q{~V:{X~}},
# Q{~V@:{~A~}},
# Q{~V{FOO~:}},
# Q{~V{~A~}},
# Q{~V{~}},
# Q{~v:@{~A~}},
# Q{~v:{ABC~:}},
# Q{~v:{~A~:}},
# Q{~v:{~A~}},
# Q{~v@{~A~}},
# Q{~v@{~}},
# Q{~v{~A~}},
# Q{~v{~a~}},
# Q{~{FOO~:}},
# Q{~{X ~A~^ Y ~A~^ ~}},
# Q{~{~#,#,#^~A~}},
# Q{~{~#,#,v^~A~}},
# Q{~{~#,#^~A~}},
# Q{~{~#,1,2^~A~}},
# Q{~{~#,3^~A~}},
# Q{~{~',,',^~A~}},
# Q{~{~'X,v^~A~}},
# Q{~{~'X^~A~}},
# Q{~{~(~C~C~0^~C~)W~}},
# Q{~{~0,v,v^~A~}},
# Q{~{~0,v^~A~}},
# Q{~{~1,1,v^~A~}},
# Q{~{~1,2,v^~A~}},
# Q{~{~1,v,v^~A~}},
# Q{~{~1,v^~A~}},
# Q{~{~1{~A~}~}},
# Q{~{~:(~C~C~0^~C~)U~}},
# Q{~{~@(~CA ~Cb ~0^~C~)V~}},
# Q{~{~@:(~CA ~Cb ~0^~C~)W~}},
# Q{~{~A~:}},
# Q{~{~A~@?~A~}},
# Q{~{~A~A~0^~A~}},
# Q{~{~A~A~v^~A~}},
# Q{~{~[X~;Y~0^NO~;Z~;~^~]~}},
# Q{~{~[X~;Y~;Z~:;~0^~]~}},
# Q{~{~[X~;Y~;Z~;~0^~]~}},
# Q{~{~v,'X^~A~}},
# Q{~{~v,1,v^~A~}},
# Q{~{~v,v,v^~A~}},
# Q{~{~v,v^~A~}},
# Q{~{~{~A~}~}},

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~~~D~~} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Directive::Tilde.new
	];

	done-testing;
}

subtest {
# Q{1~<X~<Y~:>Z~>2},
# Q{~:<foo~;~A~;bar~A~:>},
# Q{~:<foo~@;~A~;bar~A~:>},
# Q{~:<foo~A~;~A~:>},
# Q{~:<foo~A~;~A~;bar~:>},
# Q{~:<foo~A~@;~A~:>},
# Q{~:<foo~A~@;~A~;bar~:>},
# Q{~:<~;~A~;bar~A~:>},
# Q{~:<~@;~A~;bar~A~:>},
# Q{~<X~:;Y~>~I},
# Q{~<foo~;~A~;bar~A~:>},
# Q{~<foo~@;~A~;bar~A~:>},
# Q{~<foo~A~;~A~:>},
# Q{~<foo~A~;~A~;bar~:>},
# Q{~<foo~A~@;~A~:>},
# Q{~<foo~A~@;~A~;bar~:>},
# Q{~<~:;~>~<~:>},
# Q{~<~;~A~;bar~A~:>},
# Q{~<~@;~A~;bar~A~:>},
# Q{~@<foo~;~A~;bar~A~:>},
# Q{~@<foo~@;~A~;bar~A~:>},
# Q{~@<foo~A~;~A~:>},
# Q{~@<foo~A~;~A~;bar~:>},
# Q{~@<foo~A~@;~A~:>},
# Q{~@<foo~A~@;~A~;bar~:>},
# Q{~@<~;~A~;bar~A~:>},
# Q{~@<~@;~A~;bar~A~:>},
# Q{~w~<X~:;Y~>},

	done-testing;
}

subtest {
# Q{AAAA~1,1:TBBB~<XXX~:;YYY~>ZZZ},
# Q{~<XXX~1,1:TYYY~>},
# Q{~<XXX~:;YYY~>ZZZ~4,5:tWWW},

	done-testing;
}
}
