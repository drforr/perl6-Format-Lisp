use v6;

use Test;
use Format::Lisp;
plan 30;

my $fl = Format::Lisp.new;
my $*CONSISTENCY-CHECK = True;
my $*FALL-THROUGH = True;

# Skip the $messages on all of these, as they get out of sync too easily.

#
# It may not be apparent why I'm sorting on the last character. Mostly it's
# because figuring out what directive the first dirctive of a string is
# amounts to building a fairly complex regexp.
#

# XXX No !" tests?
# XXX No "" tests?
# XXX No #" tests?
# XXX No $" tests?

subtest {
#`(
	is-deeply $fl._parse( Q{X~#%} ),
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::Percent.new( options => ['#'] )
	;
	is-deeply $fl._parse( Q{X~V%} ), [
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::Percent.new( options => ['V'] ),
	];
	is-deeply $fl._parse( Q{~#%} ), [
		Format::Lisp::Directive::Percent.new( options => ['#'] ),
	];
)
	is-deeply $fl._parse( Q{~%} ), [
		Format::Lisp::Directive::Percent.new
	];
	is-deeply $fl._parse( Q{~@_A~%} ), [
		Format::Lisp::Directive::Under.new(
			at => True
		),
		Format::Lisp::Text.new( text => 'A' ),
		Format::Lisp::Directive::Percent.new
	];
#`(
	is-deeply $fl._parse( Q{~V%} ), [
		Format::Lisp::Directive::Percent.new( options => ['V'] ),
	];
)
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
	is-deeply $fl._parse( Q{X~&} ), [
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::Amp.new
	];
#`(
	is-deeply $fl._parse( Q{X~v&} ), [
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::Amp.new( options => ['v'] ),
	];
)
	is-deeply $fl._parse( Q{X~~~D&} ), [
		Format::Lisp::Text.new( text => 'X' ),
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '&' )
	];
#`(
	is-deeply $fl._parse( Q{~#&} ), [
		Format::Lisp::Directive::Amp.new( options => ['#'] ),
	];
)
	is-deeply $fl._parse( Q{~&} ), [
		Format::Lisp::Directive::Amp.new
	];
#`(
	is-deeply $fl._parse( Q{~0&} ), [
		Format::Lisp::Directive::Amp.new( options => [0] ),
	];
	is-deeply $fl._parse( Q{~v&} ), [
		Format::Lisp::Directive::Amp.new( options => ['v'] ),
	];
)
	is-deeply $fl._parse( Q{~~~D%} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '%' )
	];

	done-testing;
}

# XXX No `" tests?

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
# Q{~(XXyy~AuuVV~)},
# Q{~(aBc ~(def~) GHi~)},
# Q{~(aBc ~:(def~) GHi~)},
# Q{~(aBc ~@(def~) GHi~)},
# Q{~(~c~)},
# Q{~:(aBc ~(def~) GHi~)},
# Q{~:(aBc ~:(def~) GHi~)},
# Q{~:(aBc ~@(def~) GHi~)},
# Q{~:(aBc ~@:(def~) GHi~)},
# Q{~:(this is a TEST.~)},
# Q{~:(this is7a TEST.~)},
# Q{~:@(aBc ~(def~) GHi~)},
# Q{~:@(aBc ~@(def~) GHi~)},
# Q{~:@(this is AlSo A teSt~)},
# Q{~@(!@#$%^&*this is a TEST.~)},
# Q{~@(aBc ~(def~) GHi~)},
# Q{~@(aBc ~:(def~) GHi~)},
# Q{~@(aBc ~@(def~) GHi~)},
# Q{~@(aBc ~@:(def~) GHi~)},
# Q{~@(this is a TEST.~)},
# Q{~@:(aBc ~:(def~) GHi~)},
# Q{~@:(aBc ~@:(def~) GHi~)},
# Q{~@:(~c~)},

	done-testing;
}

# XXX No *" tests?
# XXX No +" tests?

subtest {
	is-deeply $fl._parse( Q{'~c,} ), [
		Format::Lisp::Text.new( text => Q{'} ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Text.new( text => ',' )
	];
	is-deeply $fl._parse( Q{~d,} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ',' ),
	];
	is-deeply $fl._parse( Q{~~~d,} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ',' )
	];

	done-testing;
}

# XXX No -" tests?
# XXX No ." tests?

subtest {
# Q{~',@/cl-test::function-for-format-slash-19/},
# Q{~'X:/cl-test::function-for-format-slash-19/},
# Q{~-1@/cl-test::function-for-format-slash-19/},
# Q{~/CL-TEST::FUNCTION-FOR-FORMAT-SLASH-9/},
# Q{~/PPRINT-LINEAR/},
# Q{~/cL-tESt:FUNCTION:FOR::FORMAT:SLASH:11/},
# Q{~/cl-test::function-for-format-slash-19/},
# Q{~/cl-test:FUNCTION-FOR-FORMAT-SLASH-10/},
# Q{~/pPrINt-lINeaR/},
# Q{~/pprint-linear/},
# Q{~1,2,3,4,5,6,7,8,9,10@/cl-test::function-for-format-slash-19/},
# Q{~18@:/cl-test::function-for-format-slash-19/},
# Q{~:/cl-test::function-for-format-slash-19/},
# Q{~:/pprint-linear/},
# Q{~:@/cl-test::function-for-format-slash-19/},
# Q{~@/cl-test::function-for-format-slash-19/},
# Q{~@/pprint-linear/},
# Q{~@:/cl-test::function-for-format-slash-19/},
# Q{~@:/pprint-linear/},
# Q{~v,v,v,v,v,v,v,v,v,v@/cl-test::function-for-format-slash-19/},
# Q{~v/cl-test::function-for-format-slash-19/},

	done-testing;
}

# XXX No [0-9]" tests?
# XXX No :" tests?
# XXX No ;" tests?
# XXX No <" tests?
# XXX No =" tests?

subtest {
# Q{XXX~<MMM~-1I~:@_MMMMM~:>},
# Q{XXX~<MMM~1I~:@_MMMMM~:>},
# Q{XXX~<MMM~I~:@_MMMMM~:>},
# Q{XXX~<MMM~vI~:@_MMMMM~:>},
# Q{XXX~<MMM~vI~:@_MMMMM~:>},
# Q{~%X ~,,1<~%X ~:;AAA~;BBB~;CCC~>},
# Q{~%X ~<~%X ~0,30:;AAA~>~<~%X ~0,30:;BBB~>~<~%X ~0,30:;CCC~>},
# Q{~%X ~<~%X ~0,3:;AAA~>,~<~%X ~0,3:;BBB~>,~<~%X ~0,3:;CCC~>},
# Q{~%X ~<~%X ~0,3:;AAA~>~<~%X ~0,3:;BBB~>~<~%X ~0,3:;CCC~>},
# Q{~,,1,',<~A~;~A~>},
# Q{~,,1,v<~A~;~A~>},
# Q{~,,1<~A~;~A~>},
# Q{~,,2<~A~;~A~>},
# Q{~,,v<~A~;~A~>},
# Q{~,v<~A~;~A~>},
# Q{~,v<~A~>},
# Q{~10:<abcdef~>},
# Q{~10:@<abcdef~>},
# Q{~10@<abcdef~>},
# Q{~13,,2<aaa~;bbb~;ccc~>},
# Q{~4@<~>},
# Q{~5:@<~>},
# Q{~6:<~>},
# Q{~6<abc~;def~^~>},
# Q{~6@<abc~;def~^~>},
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
# Q{~<XXXXXX~;YYYYYYY~^~;ZZZZZ~>},
# Q{~<XXXXXX~;YYYYYYY~^~>},
# Q{~<XXXXXX~^~>},
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
# Q{~<~<XXXXXX~;YYYYYYY~^~>~>},
# Q{~<~<~A~^xxx~:>yyy~:>},
# Q{~<~>},
# Q{~<~@/pprint-tabular/~:>},
# Q{~<~@{~A~^*~}~:>},
# Q{~<~A~:>},
# Q{~<~A~;~A~>},
# Q{~<~A~>},
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
# Q{~v,,,v<~A~>},
# Q{~v,,v<~A~>},
# Q{~v<~A~>},
# Q{~~~d,,,'~c<~~A~~>}, # Actually not a <>

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~?} ), [
		Format::Lisp::Directive::Ques.new( options => [] )
	];
	is-deeply $fl._parse( Q{~@?} ), [
		Format::Lisp::Directive::Ques.new(
			at => True
		)
	];

	done-testing;
}

subtest {
# Q{~#,#@A},
#`(
	is-deeply $fl._parse( Q{~#,#A} ), [
		Format::Lisp::Directive::A.new( options => ['#','#'] ),
	];
)
# Q{~#@A},
# Q{~#@a},
#`(
	is-deeply $fl._parse( Q{~#A} ), [
		Format::Lisp::Directive::A.new( options => ['#'] ),
	];
	is-deeply $fl._parse( Q{~#a} ), [
		Format::Lisp::Directive::A.new( options => ['#'] ),
	];
	is-deeply $fl._parse( Q{~-100000000000000000000a} ), [
		Format::Lisp::Directive::A.new(
			options => [ -100000000000000000000 ]
		),
	];
	is-deeply $fl._parse( Q{~-100a} ), [
		Format::Lisp::Directive::A.new( options => [-100] ),
	];
)
# Q{~10,,,v@A},
# Q{~10,,,v@a},
#`(
	is-deeply $fl._parse( Q{~-10,,,vA} ), [
		Format::Lisp::Directive::A.new(
			options => [-10,Any,Any,Any,'v']
		),
	];
	is-deeply $fl._parse( Q{~-10,,,va} ), [
		Format::Lisp::Directive::A.new(
			options => [-10,Any,Any,Any,'v']
		),
	];
	is-deeply $fl._parse( Q{~3,,+2A} ), [
		Format::Lisp::Directive::A.new( options => [3,Any,Any,2] ),
	];
	is-deeply $fl._parse( Q{~3,,-1A} ), [
		Format::Lisp::Directive::A.new( options => [3,Any,Any,-1] ),
	];
	is-deeply $fl._parse( Q{~3,,0A} ), [
		Format::Lisp::Directive::A.new( options => [3,Any,Any,0] ),
	];
)
# Q{~3,,v@A},
#`(
	is-deeply $fl._parse( Q{~3,,vA} ), [
		Format::Lisp::Directive::A.new( options => [3,Any,Any,'v'] ),
	];
	is-deeply $fl._parse( Q{~3,1a} ), [
		Format::Lisp::Directive::A.new( options => [3,1] ),
	];
)
# Q{~3,3@a},
# Q{~4,#@A},
#`(
	is-deeply $fl._parse( Q{~4,#A} ), [
		Format::Lisp::Directive::A.new( options => [4,'#'] ),
	];
)
# Q{~4,,,'X@a},
# Q{~4,,,'XA},
# Q{~4,,,@A},
#`(
	is-deeply $fl._parse( Q{~4,,,a} ), [
		Format::Lisp::Directive::A.new( options => [4,Any,Any,Any] ),
	];
	is-deeply $fl._parse( Q{~4,,va} ), [
		Format::Lisp::Directive::A.new( options => [4,Any,'v'] ),
	];
	is-deeply $fl._parse( Q{~4,3a} ), [
		Format::Lisp::Directive::A.new( options => [4,3] ),
	];
)
# Q{~4,4@a},
# Q{~5,#@A},
#`(
	is-deeply $fl._parse( Q{~5,#a} ), [
		Format::Lisp::Directive::A.new( options => [5,'#'] ),
	];
# Q{~5,3@a},
	is-deeply $fl._parse( Q{~5,3A} ), [
		Format::Lisp::Directive::A.new( options => [5,3] ),
	];
# Q{~5,v@A},
	is-deeply $fl._parse( Q{~5,vA} ), [
		Format::Lisp::Directive::A.new( options => [5,'v'] ),
	];
# Q{~7,3@a},
	is-deeply $fl._parse( Q{~7,3A} ), [
		Format::Lisp::Directive::A.new( options => [7,3] ),
	];
)
# Q{~:A},
# Q{~:a},
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
	is-deeply $fl._parse( Q{~@A} ), [
		Format::Lisp::Directive::A.new(
			at => True
		)
	];
# Q{~@[X~]Y~A},
	is-deeply $fl._parse( Q{~@a} ), [
		Format::Lisp::Directive::A.new(
			at => True
		)
	];
# Q{~@{~2,#^~A~}X~A},
	is-deeply $fl._parse( Q{~A} ), [
		Format::Lisp::Directive::A.new
	];
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
#`(
	is-deeply $fl._parse( Q{~A~0*~A} ), [
		Format::Lisp::Directive::A.new( options => [] ),
		Format::Lisp::Directive::Star.new( options => [0] ),
		Format::Lisp::Directive::A.new( options => [] ),
	];
)
# Q{~A~1{~A~*~A~}~A},
# Q{~A~1{~A~0*~A~}~A},
# Q{~A~1{~A~:*~A~}~A},
# Q{~A~1{~A~A~A~2:*~A~A~}~A},
# Q{~A~1{~A~A~A~:*~A~}~A},
# Q{~A~1{~A~A~v@*~A~A~}~A},
# Q{~A~:*~A},
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
# Q{~A~A~0:*~A},
# Q{~A~A~1@*~A~A},
# Q{~A~A~2:*~A},
# Q{~A~A~2@*~A~A},
# Q{~A~A~3@*~A~A},
# Q{~A~A~:*~A},
	is-deeply $fl._parse( Q{~A~A~@*~A~A} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Star.new(
			at => True
		),
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::A.new
	];
# Q{~A~A~v:*~A},
# Q{~A~A~v@*~A~A},
#`(
	is-deeply $fl._parse( Q{~A~v*~A} ), [
		Format::Lisp::Directive::A.new( options => [] ),
		Format::Lisp::Directive::Star.new( options => ['v'] ),
		Format::Lisp::Directive::A.new( options => [] ),
	];
)
# Q{~A~{~A~*~A~}~A},
# Q{~A~{~A~A~0@*~A~A~}~A},
# Q{~A~{~A~A~1@*~A~}~A},
# Q{~A~{~A~A~@*~A~A~}~A},
# Q{~A~{~A~A~A~3:*~A~A~A~A~}~A},
# Q{~A~{~A~A~A~A~4:*~^~A~A~A~A~}~A},
# Q{~A~{~A~A~A~A~v*~^~A~A~A~A~}~A},
# Q{~A~{~A~A~A~A~v:*~^~A~}~A},
# Q{~V:@A},
# Q{~V:@a},
# Q{~V:A},
# Q{~V:a},
# Q{~V@:A},
# Q{~V@:a},
# Q{~V@A},
# Q{~V@a},
#`(
	is-deeply $fl._parse( Q{~VA} ), [
		Format::Lisp::Directive::A.new( options => ['V'] ),
	];
	is-deeply $fl._parse( Q{~Va} ), [
		Format::Lisp::Directive::A.new( options => ['V'] ),
	];
)
	is-deeply $fl._parse( Q{~a} ), [
		Format::Lisp::Directive::A.new
	];
#`(
	is-deeply $fl._parse( Q{~v,,2A} ), [
		Format::Lisp::Directive::A.new( options => ['v',Any,Any,2] ),
	];
# Q{~v:@A},
# Q{~v:@a},
# Q{~v:A},
# Q{~v:a},
# Q{~v@:A},
# Q{~v@:a},
# Q{~v@A},
# Q{~v@a},
	is-deeply $fl._parse( Q{~vA} ), [
		Format::Lisp::Directive::A.new( options => ['v'] ),
	];
	is-deeply $fl._parse( Q{~va} ), [
		Format::Lisp::Directive::A.new( options => ['v'] ),
	];
# Q{~{~2,#^~A~}~A},
)
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
	is-deeply $fl._parse( Q{~b} ), [
		Format::Lisp::Directive::B.new
	];
	is-deeply $fl._parse( Q{~@b} ), [
		Format::Lisp::Directive::B.new(
			at => True
		)
	];
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
#`(
	is-deeply $fl._parse( Q{~v,vb} ), [
		Format::Lisp::Directive::B.new( options => ['v','v'] ),
	];
# Q{~:b},
# Q{~,,v:b},
# Q{~,,V,V:b},
# Q{~,,v,v:@b},
	is-deeply $fl._parse( Q{~vb} ), [
		Format::Lisp::Directive::B.new( options => ['v'] ),
	];
# Q{~,,v:b},
# Q{~:@b},
	is-deeply $fl._parse( Q{~#b} ), [
		Format::Lisp::Directive::B.new( options => ['#'] ),
	];

# Q{~,,,#:b},
	is-deeply $fl._parse( Q{~+10b} ), [
		Format::Lisp::Directive::B.new( options => [10] ),
	];
	is-deeply $fl._parse( Q{~-1b} ), [
		Format::Lisp::Directive::B.new( options => [-1] ),
	];
	is-deeply $fl._parse( Q{~vb} ), [
		Format::Lisp::Directive::B.new( options => ['v'] ),
	];
	is-deeply $fl._parse( Q{~db} ), [
		Format::Lisp::Directive::D.new( options => [] ),
		Format::Lisp::Text.new( text => 'b' ),
	];
	is-deeply $fl._parse( Q{~v,v,v,vb} ), [
		Format::Lisp::Directive::B.new( options => ['v','v','v','v'] ),
	];
)
	is-deeply $fl._parse( Q{~B} ), [
		Format::Lisp::Directive::B.new
	];
	is-deeply $fl._parse( Q{~@B} ), [
		Format::Lisp::Directive::B.new(
			at => True
		)
	];
#`(
	is-deeply $fl._parse( Q{~v,vb} ), [
		Format::Lisp::Directive::B.new( options => ['v','v'] ),
	];
# Q{~:B},
# Q{~,,v:B},
# Q{~,,V,V@:B},
# Q{~,,v,v:B},
	is-deeply $fl._parse( Q{~6,vB} ), [
		Format::Lisp::Directive::B.new( options => [6,'v'] ),
	];
# Q{~,,'*,v:B},
# Q{~:B},
# Q{~@:B},
	is-deeply $fl._parse( Q{~#B} ), [
		Format::Lisp::Directive::B.new( options => ['#'] ),
	];

# Q{~,,,#:B},
# Q{~,,,#@:B},
# Q{~+10@B},
	is-deeply $fl._parse( Q{~-100000000000000000000B} ), [
		Format::Lisp::Directive::B.new(
			options => [ -100000000000000000000 ]
		),
	];
	is-deeply $fl._parse( Q{~V,V,V,VB} ), [
		Format::Lisp::Directive::B.new( options => ['V','V','V','V'] ),
	];
)

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
# Q{~:c},
	is-deeply $fl._parse( Q{~@c} ), [
		Format::Lisp::Directive::C.new(
			at => True
		)
	];
	is-deeply $fl._parse( Q{'~c} ), [
		Format::Lisp::Text.new( text => Q{'} ),
		Format::Lisp::Directive::C.new
	];
# Q{#\\~:c},
	is-deeply $fl._parse( Q{~c} ), [
		Format::Lisp::Directive::C.new
	];
# Q{~@:c},
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
	is-deeply $fl._parse( Q{~C} ), [
		Format::Lisp::Directive::C.new
	];
# Q{~:C},
	is-deeply $fl._parse( Q{~@C} ), [
		Format::Lisp::Directive::C.new(
			at => True
		)
	];
# Q{~:@C},
	is-deeply $fl._parse( Q{~c} ), [
		Format::Lisp::Directive::C.new
	];
# Q{~:C},
# Q{~@:C},

	done-testing;
}

subtest {
	is-deeply $fl._parse( Q{~@d} ), [
		Format::Lisp::Directive::D.new(
			at => True
		)
	];
	is-deeply $fl._parse( Q{~~~dd} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'd' )
	];
	is-deeply $fl._parse( Q{~d} ), [
		Format::Lisp::Directive::D.new
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
#`(
	is-deeply $fl._parse( Q{~v,vd} ), [
		Format::Lisp::Directive::D.new( options => ['v','v'] ),
	];
)
# Q{~v,v@d},
# Q{~:d},
# Q{~,,v:d},
# Q{~,,v:d},
	is-deeply $fl._parse( Q{~~,,'~c:d} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Text.new( text => Q{,,'} ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Text.new( text => ':d' )
	];
#`(
# Q{~,,v,v:d},
# Q{~,,v,v:@d},
# Q{~,,v:d},
# Q{~,,'*,v:d},
# Q{~@:d},
	is-deeply $fl._parse( Q{~#d} ), [
		Format::Lisp::Directive::D.new( options => ['#'] ),
	];
# Q{~,,,#:d},
# Q{~,,,#:@d},
	is-deeply $fl._parse( Q{~+10d} ), [
		Format::Lisp::Directive::D.new( options => [10] ),
	];
# Q{~+10@d},
	is-deeply $fl._parse( Q{~-1d} ), [
		Format::Lisp::Directive::D.new( options => [-1] ),
	];
	is-deeply $fl._parse( Q{~-100000000000000000000d} ), [
		Format::Lisp::Directive::D.new(
			options => [ -100000000000000000000 ]
		),
	];
	is-deeply $fl._parse( Q{~vd} ), [
		Format::Lisp::Directive::D.new( options => ['v'] ),
	];
)
	is-deeply $fl._parse( Q{~dd} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'd' ),
	];
#`(
	is-deeply $fl._parse( Q{~v,v,v,vd} ), [
		Format::Lisp::Directive::D.new( options => ['v','v','v','v'] ),
	];
# Q{~,,,#@:D},
	is-deeply $fl._parse( Q{~v,v,v,vD} ), [
		Format::Lisp::Directive::D.new( options => ['v','v','v','v'] ),
	];
)
	is-deeply $fl._parse( Q{~D} ), [
		Format::Lisp::Directive::D.new
	];
	is-deeply $fl._parse( Q{~@D} ), [
		Format::Lisp::Directive::D.new(
			at => True
		)
	];
#`(
	is-deeply $fl._parse( Q{~v,vD} ), [
		Format::Lisp::Directive::D.new( options => ['v','v'] ),
	];
# Q{~v,v@D},
# Q{~,,v,v:D},
# Q{~,,v,v:@D},
	is-deeply $fl._parse( Q{~vD} ), [
		Format::Lisp::Directive::D.new( options => ['v'] ),
	];
	is-deeply $fl._parse( Q{~6,vD} ), [
		Format::Lisp::Directive::D.new( options => [6,'v'] ),
	];
	is-deeply $fl._parse( Q{~#D} ), [
		Format::Lisp::Directive::D.new( options => ['#'] ),
	];
# Q{~,,,#:D},
)

	done-testing;
}

# XXX No ~e tests?

subtest {
#`(
	is-deeply $fl._parse( Q{~,,,,',f} ), [
		Format::Lisp::Directive::F.new( options => ['\''] ),
	];
	is-deeply $fl._parse( Q{~,,,,VF} ), [
		Format::Lisp::Directive::F.new( options => ['V'] ),
	];
	is-deeply $fl._parse( Q{~,,,,vf} ), [
		Format::Lisp::Directive::F.new( options => ['v'] ),
	];
	is-deeply $fl._parse( Q{~,,,vF} ), [
		Format::Lisp::Directive::F.new( options => ['v'] ),
	];
	is-deeply $fl._parse( Q{~,,2F} ), [
		Format::Lisp::Directive::F.new( options => [2] ),
	];
	is-deeply $fl._parse( Q{~,,Vf} ), [
		Format::Lisp::Directive::F.new( options => ['V'] ),
	];
	is-deeply $fl._parse( Q{~,,vf} ), [
		Format::Lisp::Directive::F.new( options => ['v'] ),
	];
	is-deeply $fl._parse( Q{~,,2f} ), [
		Format::Lisp::Directive::F.new( options => [2] ),
	];
	is-deeply $fl._parse( Q{~,2F} ), [
		Format::Lisp::Directive::F.new( options => [2] ),
	];
	is-deeply $fl._parse( Q{~,vf} ), [
		Format::Lisp::Directive::F.new( options => ['v'] ),
	];
	is-deeply $fl._parse( Q{~0,0f} ), [
		Format::Lisp::Directive::F.new( options => [0,0] ),
	];
	is-deeply $fl._parse( Q{~0f} ), [
		Format::Lisp::Directive::F.new( options => [0] ),
	];
	is-deeply $fl._parse( Q{~1,1,,f} ), [
		Format::Lisp::Directive::F.new( options => [1,1,Any,Any] ),
	];
	is-deeply $fl._parse( Q{~10,1,,,'*F} ), [
		Format::Lisp::Directive::F.new( options => [10,1,Any,Any,'*'] ),
	];
	is-deeply $fl._parse( Q{~10,1,,,'*f} ), [
		Format::Lisp::Directive::F.new( options => [10,1,Any,Any,'*'] ),
	];
	is-deeply $fl._parse( Q{~10,1,,f} ), [
		Format::Lisp::Directive::F.new( options => [10,1,Any,Any] ),
	];
	is-deeply $fl._parse( Q{~2,1F} ), [
		Format::Lisp::Directive::F.new( options => [2,1] ),
	];
	is-deeply $fl._parse( Q{~2,1f} ), [
		Format::Lisp::Directive::F.new( options => [2,1] ),
	];
	is-deeply $fl._parse( Q{~2,2F} ), [
		Format::Lisp::Directive::F.new( options => [2,1] ),
	];
	is-deeply $fl._parse( Q{~2,2f} ), [
		Format::Lisp::Directive::F.new( options => [2,1] ),
	];
	is-deeply $fl._parse( Q{~2f} ), [
		Format::Lisp::Directive::F.new( options => [2,1] ),
	];
	is-deeply $fl._parse( Q{~3,2F} ), [
		Format::Lisp::Directive::F.new( options => [2,1] ),
	];
	is-deeply $fl._parse( Q{~3,2f} ), [
		Format::Lisp::Directive::F.new( options => [2,1] ),
	];
# Q{~3@F},
	is-deeply $fl._parse( Q{~3F} ), [
		Format::Lisp::Directive::F.new( options => [3] ),
	];
	is-deeply $fl._parse( Q{~3f} ), [
		Format::Lisp::Directive::F.new( options => [3] ),
	];
	is-deeply $fl._parse( Q{~4,0,,'*f} ), [
		Format::Lisp::Directive::F.new( options => [4,0,Any,'*'] ),
	];
	is-deeply $fl._parse( Q{~4,2,-1F} ), [
		Format::Lisp::Directive::F.new( options => [4,2,-1] ),
	];
	is-deeply $fl._parse( Q{~4,2,-1f} ), [
		Format::Lisp::Directive::F.new( options => [4,2,-1] ),
	];
	is-deeply $fl._parse( Q{~4,2,0F} ), [
		Format::Lisp::Directive::F.new( options => [4,2,0] ),
	];
	is-deeply $fl._parse( Q{~4,2,0f} ), [
		Format::Lisp::Directive::F.new( options => [4,2,0] ),
	];
	is-deeply $fl._parse( Q{~4,2,1f} ), [
		Format::Lisp::Directive::F.new( options => [4,2,1] ),
	];
# Q{~4,2@F},
# Q{~4,2@f},
	is-deeply $fl._parse( Q{~4,2F} ), [
		Format::Lisp::Directive::F.new( options => [4,2] ),
	];
	is-deeply $fl._parse( Q{~4,2f} ), [
		Format::Lisp::Directive::F.new( options => [4,2] ),
	];
# Q{~4@F},
# Q{~4@f},
	is-deeply $fl._parse( Q{~4F} ), [
		Format::Lisp::Directive::F.new( options => [4] ),
	];
	is-deeply $fl._parse( Q{~4f} ), [
		Format::Lisp::Directive::F.new( options => [4] ),
	];
	is-deeply $fl._parse( Q{~5,1,,'*F} ), [
		Format::Lisp::Directive::F.new( options => [5,1,Any,'*'] ),
	];
	is-deeply $fl._parse( Q{~5,1,,'*f} ), [
		Format::Lisp::Directive::F.new( options => [5,1,Any,'*'] ),
	];
)
	is-deeply $fl._parse( Q{~F} ), [
		Format::Lisp::Directive::F.new( options => [] ),
	];
#`(
	is-deeply $fl._parse( Q{~VF} ), [
		Format::Lisp::Directive::F.new( options => ['V'] ),
	];
)
	is-deeply $fl._parse( Q{~f} ), [
		Format::Lisp::Directive::F.new
	];
#`(
	is-deeply $fl._parse( Q{~v,v,v,vf} ), [
		Format::Lisp::Directive::F.new( options => ['v','v','v','v'] ),
	];
	is-deeply $fl._parse( Q{~v,vf} ), [
		Format::Lisp::Directive::F.new( options => ['v','v'] ),
	];
	is-deeply $fl._parse( Q{~vf} ), [
		Format::Lisp::Directive::F.new( options => ['v'] ),
	];
)
	is-deeply $fl._parse( Q{~~,,,,'~cf} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Text.new( text => ',,,,\'' ),
		Format::Lisp::Directive::C.new,
		Format::Lisp::Text.new( text => 'f' ),
	];

	done-testing;
}

subtest {
#`(
# Q{~#o},
	is-deeply $fl._parse( Q{~#&} ), [
		Format::Lisp::Directive::Amp.new( options => ['#'] ),
	];

# Q{~+10@O},
# Q{~+10o},
	is-deeply $fl._parse( Q{~+10o} ), [
		Format::Lisp::Directive::O.new( options => [10] ),
	];
# Q{~,,'*,v:o},
# Q{~,,,#:@o},
# Q{~,,,#:o},
# Q{~,,,#@:O},
# Q{~,,V,v:O},
# Q{~,,v,V@:O},
# Q{~,,v,v:@o},
# Q{~,,v,v:O},
# Q{~,,v:o},
	is-deeply $fl._parse( Q{~-100000000000000000000o} ), [
		Format::Lisp::Directive::O.new(
			options => [ -100000000000000000000 ]
		),
	];
	is-deeply $fl._parse( Q{~-1O} ), [
		Format::Lisp::Directive::O.new( options => [-1] ),
	];
	is-deeply $fl._parse( Q{~6,vO} ), [
		Format::Lisp::Directive::O.new( options => [6,'v'] ),
	];
)
# Q{~:@o},
# Q{~:O},
# Q{~:o},
# Q{~@:o},
	is-deeply $fl._parse( Q{~@O} ), [
		Format::Lisp::Directive::O.new(
			at => True
		)
	];
	is-deeply $fl._parse( Q{~@o} ), [
		Format::Lisp::Directive::O.new(
			at => True
		)
	];
	is-deeply $fl._parse( Q{~O} ), [
		Format::Lisp::Directive::O.new
	];
#`(
	is-deeply $fl._parse( Q{~V,Vo} ), [
		Format::Lisp::Directive::O.new( options => ['V','V'] ),
	];
)
	is-deeply $fl._parse( Q{~o} ), [
		Format::Lisp::Directive::O.new
	];
#`(
# Q{~v,V@O},
# Q{~v,v,v,vo},
	is-deeply $fl._parse( Q{~v,v,v,vo} ), [
		Format::Lisp::Directive::O.new( options => ['v','v','v','v'] ),
	];
# Q{~v,v@o},
	is-deeply $fl._parse( Q{~v,vO} ), [
		Format::Lisp::Directive::O.new( options => ['v','v'] ),
	];
	is-deeply $fl._parse( Q{~vO} ), [
		Format::Lisp::Directive::O.new( options => ['v'] ),
	];
	is-deeply $fl._parse( Q{~vo} ), [
		Format::Lisp::Directive::O.new( options => ['v'] ),
	];
)
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
# Q{~@p},
# Q{~D cat~:p},
# Q{~D penn~:@p},
# Q{~D penn~@:p},
	is-deeply $fl._parse( Q{~p} ), [
		Format::Lisp::Directive::P.new
	];
# Q{~@P},
# Q{~D cat~:P},
# Q{~D penn~:@P},
# Q{~D penn~@:P},
	is-deeply $fl._parse( Q{~P} ), [
		Format::Lisp::Directive::P.new
	];

	done-testing;
}

# XXX No trailing-q tests?

subtest {
#`(
	is-deeply $fl._parse( Q{~#r} ), [
		Format::Lisp::Directive::R.new( options => ['#'] ),
	];
	is-deeply $fl._parse( Q{~+10r} ), [
		Format::Lisp::Directive::R.new( options => [10] ),
	];
	is-deeply $fl._parse( Q{~10,#r} ), [
		Format::Lisp::Directive::R.new( options => [10,'#'] ),
	];
	is-deeply $fl._parse( Q{~10,+8r} ), [
		Format::Lisp::Directive::R.new( options => [10,8] ),
	];
# Q{~10,,,v:r},
# Q{~10,-1000000000000000r},
	is-deeply $fl._parse( Q{~10,-1r} ), [
		Format::Lisp::Directive::R.new( options => [10,-1] ),
	];
	is-deeply $fl._parse( Q{~10,0r} ), [
		Format::Lisp::Directive::R.new( options => [10,0] ),
	];
# Q{~10,12,vr},
	is-deeply $fl._parse( Q{~10,vR} ), [
		Format::Lisp::Directive::R.new( options => [10,'v'] ),
	];
	is-deeply $fl._parse( Q{~10,vr} ), [
		Format::Lisp::Directive::R.new( options => [10,'v'] ),
	];
	is-deeply $fl._parse( Q{~10r} ), [
		Format::Lisp::Directive::R.new( options => [10] ),
	];
# Q{~16,,,,#:r},
# Q{~2,,,,1000000000000000000r},
# Q{~2,12,,'*:r},
# Q{~2:r},
	is-deeply $fl._parse( Q{~2r} ), [
		Format::Lisp::Directive::R.new( options => [2] ),
	];
# Q{~3@:r},
	is-deeply $fl._parse( Q{~3r} ), [
		Format::Lisp::Directive::R.new( options => [3] ),
	];
)
# Q{~8,10:@r},
# Q{~:@r},
# Q{~:r},
	is-deeply $fl._parse( Q{~@r} ), [
		Format::Lisp::Directive::R.new(
			at => True
		)
	];
	is-deeply $fl._parse( Q{~dr} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'r' )
	];
	is-deeply $fl._parse( Q{~r} ), [
		Format::Lisp::Directive::R.new
	];
#`(
# Q{~v,v,v,v,vr},
	is-deeply $fl._parse( Q{~vr} ), [
		Format::Lisp::Directive::R.new( options => ['v'] ),
	];
)
	is-deeply $fl._parse( Q{~~~D,~D,'*r} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ',' ),
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => Q{,'*r} ),
	];
# Q{~3,14,'X,',:R},
# Q{~8,,,,v:R},
# Q{~8@R},
# Q{~@:R},
	is-deeply $fl._parse( Q{~@R} ), [
		Format::Lisp::Directive::R.new(
			at => True
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
# Q{~10,,,v@s},
# Q{~10,,,vs},
# Q{~3,,vs},
# Q{~3,1s},
# Q{~3,3@s},
# Q{~4,,,'X@s},
# Q{~4,,,s},
# Q{~4,,vs},
# Q{~4,3s},
# Q{~4,4@s},
# Q{~5,3@s},
# Q{~7,3@s},
# Q{~:s},
# Q{~V,,2s},
# Q{~V:s},
# Q{~V@:s},
	is-deeply $fl._parse( Q{~s} ), [
		Format::Lisp::Directive::S.new
	];
# Q{~v:@s},
# Q{~v:@s},
# Q{~v@:s},
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
# Q{~10,,,v@S},
# Q{~10,,,vS},
# Q{~3,,+2S},
# Q{~3,,-1S},
# Q{~3,,0S},
# Q{~3,,V@S},
# Q{~3,,vS},
# Q{~4,,,'XS},
# Q{~4,,,@S},
# Q{~5,3S},
# Q{~5,v@S},
# Q{~5,vS},
# Q{~5,vS},
# Q{~7,3S},
	is-deeply $fl._parse( Q{~@S} ), [
		Format::Lisp::Directive::S.new(
			at => True
		)
	];
	is-deeply $fl._parse( Q{~S} ), [
		Format::Lisp::Directive::S.new
	];
# Q{~v,,2S},
# Q{~v:S},
# Q{~v:S},
# Q{~v@S},
# Q{~vS},
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
# Q{~1,1@t},
# Q{~A~v,vt},
# Q{~v,1@T~0,v@t},
# Q{~v,1@t},
# Q{~v,v@t},
	is-deeply $fl._parse( Q{~~~d,~d@t} ), [
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ',' ),
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => '@t' )
	];
# Q{ ~v,vT},
# Q{XXXXX~2,0T},
# Q{~0,0T},
# Q{~0,1T},
# Q{~0,vT},
# Q{~1,0T},
	is-deeply $fl._parse( Q{~A~~~D,~DT} ), [
		Format::Lisp::Directive::A.new,
		Format::Lisp::Directive::Tilde.new,
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => ',' ),
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'T' )
	];
# Q{~v,0T},

	done-testing;
}

# XXX No trailing-u tests?
# XXX No trailing-v tests?

subtest {
#`(
	is-deeply $fl._parse( Q{~#x} ), [
		Format::Lisp::Directive::X.new( options => ['#'] ),
	];
)

# Q{~+10x},
# Q{~,,'*,v:x},
# Q{~,,,#:x},
# Q{~,,V:x},
# Q{~,,v,V:@x},
# Q{~,,v,v:@x},
# Q{~,,v:x},
# Q{~-1000000000000000000x},
# Q{~:@x},
# Q{~:x},
# Q{~@:x},
	is-deeply $fl._parse( Q{~@x} ), [
		Format::Lisp::Directive::X.new(
			at => True
		)
	];
# Q{~V,vx},
	is-deeply $fl._parse( Q{~dx} ), [
		Format::Lisp::Directive::D.new,
		Format::Lisp::Text.new( text => 'x' )
	];
# Q{~v,v,v,vx},
# Q{~v,v@x},
# Q{~vx},
	is-deeply $fl._parse( Q{~x} ), [
		Format::Lisp::Directive::X.new
	];
	is-deeply $fl._parse( Q{X} ), [
		Format::Lisp::Text.new( text => 'X' )
	];
#`(
	is-deeply $fl._parse( Q{~#X} ), [
		Format::Lisp::Directive::X.new( options => ['#'] ),
	];
)

# Q{~+10@X},
# Q{~,,,#:X},
# Q{~,,,#@:X},
# Q{~,,v,v:X},
# Q{~,,v,v:X},
# Q{~,,v:X},
# Q{~-1X},
# Q{~6,vX},
# Q{~:X},
	is-deeply $fl._parse( Q{~@X} ), [
		Format::Lisp::Directive::X.new(
			at => True
		)
	];
	is-deeply $fl._parse( Q{~X} ), [
		Format::Lisp::Directive::X.new
	];
# Q{~v,V@X},
# Q{~v,vX},

	done-testing;
}

subtest {
# Q{XX~10,20:@tYY},
# Q{XX~10,20@:tYY},
# Q{XX~10:tYY},
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

# XXX no [" tests
# XXX no \" tests

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
# Q{A ~:@_A ~:@_A ~:@_A ~:@_},
# Q{A ~:@_A ~:@_A ~:@_A ~:@_A ~:@_},
# Q{A ~:_A ~:_A ~:_A ~:_A ~:_},
# Q{A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_},
# Q{A ~@:_A ~@:_A ~@:_A ~@:_},
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
# Q{A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_},
# Q{A ~@:_A },
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
# Q{AAAA ~:@_},
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
# Q{~W~W~:_~W~W~:_~W~W~:_~W~W~:_~W~W~:_},

	done-testing;
}

subtest {
# Q{~0|},
# Q{~V|},
	is-deeply $fl._parse( Q{~|} ), [
		Format::Lisp::Directive::Pipe.new
	];
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
# Q{~:@{~#,#:^~A~}},
# Q{~:@{~#,#^~A~}},
# Q{~:@{~#,1:^~A~}},
# Q{~:@{~#,1^~A~}},
# Q{~:@{~#,2,#:^~A~}},
# Q{~:@{~#,2,2:^~A~}},
# Q{~:@{~#,3,#^~A~}},
# Q{~:@{~#,3,3^~A~}},
# Q{~:@{~#,v:^~A~}},
# Q{~:@{~#:^~A~}},
# Q{~:@{~#^~A~}},
# Q{~:@{~'X,'X:^~A~}},
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
# Q{~:@{~1,0,1^~A~}},
# Q{~:@{~1,1,1^~A~}},
# Q{~:@{~1,1,v:^~A~}},
# Q{~:@{~1,1:^~A~}},
# Q{~:@{~1,2,1:^~A~}},
# Q{~:@{~1,2,1^~A~}},
# Q{~:@{~1,2,3:^~A~}},
# Q{~:@{~1,2,3^~A~}},
# Q{~:@{~1,2,v^~A~}},
# Q{~:@{~1,3,#:^~A~}},
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
# Q{~:@{~:^~A~}},
# Q{~:@{~A~:}},
# Q{~:@{~A~^~A~A~}},
# Q{~:@{~A~}},
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
# Q{~:@{~}},
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
# Q{~:{~}},
# Q{~@:{~#^~A~#^~A~#^~A~#^~A~}},
# Q{~@:{~3,v^~A~}},
# Q{~@:{~A~0^~A~A~}},
# Q{~@{ ~}},
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
# Q{~@{~A~}},
# Q{~@{~v,'X^~A~}},
# Q{~@{~v,1,v^~A~}},
# Q{~@{~v,v,v^~A~}},
# Q{~@{~v,v^~A~}},
# Q{~@{~v,v^~A~}},
# Q{~@{~{~A~}~}},
# Q{~@{~}},
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
# Q{~{ ~}},
# Q{~{FOO~:}},
# Q{~{X Y Z~}},
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
# Q{~{~A~}},
# Q{~{~[X~;Y~0^NO~;Z~;~^~]~}},
# Q{~{~[X~;Y~;Z~:;~0^~]~}},
# Q{~{~[X~;Y~;Z~;~0^~]~}},
# Q{~{~v,'X^~A~}},
# Q{~{~v,1,v^~A~}},
# Q{~{~v,v,v^~A~}},
# Q{~{~v,v^~A~}},
# Q{~{~{~A~}~}},
# Q{~{~}},

	done-testing;
}

subtest {
#`(
	is-deeply $fl._parse( Q{~#~} ), [
		Format::Lisp::Directive::Tilde.new( options => ['#'] ),
	];
	is-deeply $fl._parse( Q{~v~} ), [
		Format::Lisp::Directive::Tilde.new( options => ['v'] ),
	];
)
	is-deeply $fl._parse( Q{~~} ), [
		Format::Lisp::Directive::Tilde.new
	];
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
# Q{~< ~W ~>},
# Q{~< ~_ ~>},
# Q{~< ~i ~>},
# Q{~<X~:;Y~>~I},
# Q{~<X~:;Y~>~W},
# Q{~<X~:;Y~>~_},
# Q{~<foo~;~A~;bar~A~:>},
# Q{~<foo~@;~A~;bar~A~:>},
# Q{~<foo~A~;~A~:>},
# Q{~<foo~A~;~A~;bar~:>},
# Q{~<foo~A~@;~A~:>},
# Q{~<foo~A~@;~A~;bar~:>},
# Q{~<~:;~>~<~:>},
# Q{~<~:>~<~:;~>},
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
# Q{~_~<X~:;Y~>},
# Q{~i~<X~:;Y~>},
# Q{~w~<X~:;Y~>},

	done-testing;
}

subtest {
# Q{~:@{~A ~A~}},
# Q{~:{~A~}},
# Q{~{~A~}},

	done-testing;
}

subtest {
# Q{~:@{~A~}},
# Q{~{~A~}},
# Q{~:@{~A~}},
# Q{~{~A~}},
# Q{~{~A~}},
# Q{~:@{~A~}},
# Q{~:{~A~}},
# Q{~:@{~A~}},
# Q{~{~A~}},

	done-testing;
}

subtest {
# Q{AAAA~1,1:TBBB~<XXX~:;YYY~>ZZZ},
# Q{~<XXX~1,1:TYYY~>},
# Q{~<XXX~:;YYY~>ZZZ~4,5:tWWW},

	done-testing;
}

subtest {
#`(
#	my @failing-options =
# Q[~{],
# Q[~}],
# Q[~(],
# Q[~)],
# Q[~<],
# Q[~>],
# Q[~;], # tilde-Semi outside balanced block
)

	done-testing;
}
