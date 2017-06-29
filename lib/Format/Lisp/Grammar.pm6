=begin pod

=begin NAME

Format::Lisp::Grammar - Grammar for Common Lisp format strings

=end NAME

=begin DESCRIPTION

=end DESCRIPTION

=end pod

grammar Format::Lisp::Grammar {
	token signed-integer {
		<[ - + ]>? <unsigned-integer>
	}
	token unsigned-integer {
	|	<[ 1..9 ]> <[ 0..9 ]>*
	|	0
	}

	token V { <[ v V ]> }

	token not-Tilde { <-[ ~ ]>+ }

	token value {
	|	'#'
	|	'\'' <[ a..z A..Z , * ]>
	|	<V>
	|	<signed-integer>
	}

	token options {
	|	'@' ':'?
	|	':' '@'?
	}

	token value-comma {
		<value>? ','
	}

	token tilde-Tilde {
		'~' <value>? '~'
	}

	token tilde-A {
		'~' <value-comma> ** 0..3 <value>? <options>? <[ a A ]>
	}

	token tilde-B {
		'~' <value-comma> ** 0..3 <value>? <options>? <[ b B ]>
	}

	token tilde-C {
		'~' <options>? <[ c C ]>
	}

	token tilde-D {
		'~' <value-comma> ** 0..3 <value>? <options>? <[ d D ]>
	}

	token tilde-F {
		'~' <value-comma> ** 0..4 <value>? <options>? <[ f F ]>
	}

	token tilde-I {
		'~' <value>? <options>? <[ i I ]>
	}

	token tilde-O {
		'~' <value-comma> ** 0..3 <value>? <options>? <[ o O ]>
	}

	token tilde-P {
		'~' <value>? <options>? <[ p P ]>
	}

	token tilde-R {
		'~' <value-comma> ** 0..4 <value>? <options>? <[ r R ]>
	}

	token tilde-S {
		'~' <value-comma> ** 0..3 <value>? <options>? <[ s S ]>
	}

	token tilde-T {
		'~' <value-comma> ** 0..1 <value>? <options>? <[ t T ]>
	}

	token tilde-W {
		'~' <value>? <options>? <[ w W ]>
	}

	token tilde-X {
		'~' <value-comma> ** 0..3 <value>? <options>? <[ x X ]>
	}

	token tilde-Caret {
		'~' <value-comma> ** 0..3 <value>? <options>? '^'
	}

	token tilde-Ques {
		'~' <value>? <options>? '?'
	}

	token tilde-Star {
		'~' <value>? <options>? '*'
	}

	token tilde-OParen {
		'~' <value>? <options>? '('
	}

	token tilde-CParen {
		'~' <value>? <options>? ')'
	}

	token tilde-OBrace {
		'~' <value>? <options>? '{'
	}

	token tilde-CBrace {
		'~' <value>? <options>? '}'
	}

	token tilde-OBracket {
		'~' <value>? <options>? '['
	}

	token tilde-Semi {
		'~' <value-comma> ** 0..1 <value>? <options>? ';'
	}

	token tilde-CBracket {
		'~' ']'
	}

	token tilde-OAngle {
		'~' <value-comma> ** 0..3 <value>? <options>? '<'
	}

	token tilde-CAngle {
		'~' <value>? <options>? '>'
	}

	token tilde-Percent {
		'~' <value>? '%'
	}

	token tilde-Pipe {
		'~' <value>? '|'
	}

	token tilde-Amp {
		'~' <value>? '&'
	}

	token tilde-Comma {
		'~' ','
	}

	token tilde-Under {
		'~' <options>? '_'
	}

	token tilde-Slash {
		'~' <value-comma> ** 0..9 <value>? <options>? '/' <-[ / ]>+ '/'
	}

	token tilde-Radix {
	|	<tilde-R>
	|	<tilde-D>
	|	<tilde-B>
	|	<tilde-O>
	|	<tilde-X>
	}

	token tilde-Printer {
	|	<tilde-A>
	|	<tilde-S>
	|	<tilde-W>
	}

	token tilde-Atom {
	|	<tilde-Radix>
	|	<tilde-Printer>
	|	<tilde-C>
	|	<tilde-Percent>
	|	<tilde-Amp>
	|	<tilde-Tilde>
	}

	token tilde-Angle {
	<tilde-OAngle>
		[
		| <not-Tilde>
		| <not-Tilde> <tilde-Angle> <not-Tilde>
		| <not-Tilde> <tilde-Atom> <not-Tilde>
		| <not-Tilde> <tilde-Atom> <tilde-Semi> <tilde-Atom>
		| <not-Tilde> <tilde-Atom> <tilde-Semi> <tilde-Atom> <tilde-Comma> <not-Tilde>
		| <not-Tilde> <tilde-Atom> <tilde-Semi> <tilde-Atom> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Caret>
		| <not-Tilde> <tilde-I> <not-Tilde>
		| <not-Tilde> <tilde-I> <tilde-Under> <not-Tilde>
		| <not-Tilde> <tilde-OAngle> <not-Tilde> <tilde-CAngle> <not-Tilde>
		| <not-Tilde> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Caret>
		| <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Caret> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-I> <tilde-Under> <not-Tilde> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-T> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Semi> <tilde-Atom> <tilde-Semi> <not-Tilde> <tilde-Atom>
		| <not-Tilde> <tilde-Semi> <tilde-Brace>
		| <not-Tilde> <tilde-Semi> <tilde-Brace> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Semi> <tilde-T>
		| <not-Tilde> <tilde-Semi> <tilde-T> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-T> <not-Tilde>
		| <not-Tilde> <tilde-Under> <not-Tilde>
		| <tilde-Angle>
		| <tilde-Angle> <not-Tilde>
		| <tilde-Atom>
		| <tilde-Atom> <not-Tilde> <tilde-Semi> <not-Tilde>
		| <tilde-Atom> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde>
		| <tilde-Atom> <tilde-Caret> <not-Tilde>
		| <tilde-Atom> <tilde-Semi> <tilde-Atom>
		| <tilde-Atom> <tilde-T>
		| <tilde-Brace>
		| <tilde-Semi>
		| <tilde-Semi> <tilde-Atom>
		| <tilde-Semi> <tilde-Atom> <tilde-Semi>
		| <tilde-Semi> <tilde-Atom> <tilde-Semi> <not-Tilde> <tilde-Atom>
		| <tilde-Semi> <tilde-Brace> <tilde-Semi> <not-Tilde>
		| <tilde-Slash>
		| <tilde-T>
		]?
	<tilde-CAngle>
	}

	token tilde-Brace {
	<tilde-OBrace>
		[
		| <not-Tilde>
		| <not-Tilde> <tilde-Atom> <not-Tilde>
		| <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
		| <not-Tilde> <tilde-Atom> <tilde-Caret> <not-Tilde> <tilde-Atom> <tilde-Caret> <not-Tilde>
		| <tilde-Atom>
		| <tilde-Atom> <not-Tilde> <tilde-Atom>
		| <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Star> <tilde-Caret> <tilde-Atom>
		| <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Star> <tilde-Caret> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		| <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Star> <tilde-Atom>
		| <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Star> <tilde-Atom> <tilde-Atom>
		| <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Star> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		| <tilde-Atom> <tilde-Atom> <tilde-Caret> <tilde-Atom>
		| <tilde-Atom> <tilde-Atom> <tilde-Star> <tilde-Atom>
		| <tilde-Atom> <tilde-Atom> <tilde-Star> <tilde-Atom> <tilde-Atom>
		| <tilde-Atom> <tilde-Caret>
		| <tilde-Atom> <tilde-Caret> <not-Tilde>
		| <tilde-Atom> <tilde-Caret> <not-Tilde> <tilde-Under>
		| <tilde-Atom> <tilde-Caret> <tilde-Atom>
		| <tilde-Atom> <tilde-Caret> <tilde-Atom> <tilde-Atom>
		| <tilde-Atom> <tilde-Ques> <tilde-Atom>
		| <tilde-Atom> <tilde-Star> <tilde-Atom>
		| <tilde-Brace>
		| <tilde-Bracket>
		| <tilde-Caret> <tilde-Atom>
		| <tilde-Caret> <tilde-Atom> <tilde-Caret> <tilde-Atom> <tilde-Caret> <tilde-Atom> <tilde-Caret> <tilde-Atom>
		| <tilde-Paren> <not-Tilde>
		]?
	<tilde-CBrace>
	}

	token tilde-Bracket {
	<tilde-OBracket>
		[
		| <not-Tilde>
		| <not-Tilde> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Caret> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <tilde-Caret>
		| <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Caret>
		| <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <tilde-Caret>
		| <tilde-Semi> <not-Tilde>
		]?
	<tilde-CBracket>
	}

	token tilde-Paren {
	<tilde-OParen>
		[
		| <not-Tilde>
		| <not-Tilde> <tilde-Atom> <not-Tilde>
		| <not-Tilde> <tilde-Paren> <not-Tilde>
		| <tilde-Atom>
		| <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Caret> <tilde-Atom>
		| <tilde-Atom> <tilde-Atom> <tilde-Caret> <tilde-Atom>
		]
	<tilde-CParen>
	}

	token TOP {
	| <not-Tilde>
	| <not-Tilde> <tilde-Angle>
	| <not-Tilde> <tilde-Angle> <not-Tilde>
	| <not-Tilde> <tilde-Atom>
	| <not-Tilde> <tilde-Atom> <not-Tilde>
	| <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
	| <not-Tilde> <tilde-Atom> <tilde-Atom>
	| <not-Tilde> <tilde-Atom> <tilde-Atom> <not-Tilde>
	| <not-Tilde> <tilde-Brace> <not-Tilde>
	| <not-Tilde> <tilde-Ques> <not-Tilde>
	| <not-Tilde> <tilde-T>
	| <not-Tilde> <tilde-T> <not-Tilde>
	| <not-Tilde> <tilde-T> <not-Tilde> <tilde-Angle> <not-Tilde>
	| <not-Tilde> <tilde-Under>
	| <not-Tilde> <tilde-Under> <not-Tilde>
	| <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under>
	| <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under>
	| <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under>
	| <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <tilde-Atom> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under>
	| <tilde-Angle>
	| <tilde-Angle> <not-Tilde> <tilde-T> <not-Tilde>
	| <tilde-Angle> <tilde-Angle>
	| <tilde-Angle> <tilde-Atom>
	| <tilde-Angle> <tilde-I>
	| <tilde-Angle> <tilde-Under>
	| <tilde-Atom>
	| <tilde-Atom> <not-Tilde>
	| <tilde-Atom> <not-Tilde> <tilde-Angle>
	| <tilde-Atom> <not-Tilde> <tilde-Angle> <not-Tilde> <tilde-Angle> <not-Tilde> <tilde-Angle>
	| <tilde-Atom> <not-Tilde> <tilde-Angle> <tilde-Angle> <tilde-Angle>
	| <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
	| <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom>
	| <tilde-Atom> <not-Tilde> <tilde-P>
	| <tilde-Atom> <not-Tilde> <tilde-Ques> <not-Tilde> <tilde-Atom>
	| <tilde-Atom> <not-Tilde> <tilde-Under>
	| <tilde-Atom> <tilde-Angle>
	| <tilde-Atom> <tilde-Atom>
	| <tilde-Atom> <tilde-Atom> <not-Tilde>
	| <tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom>
	| <tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
	| <tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
	| <tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom> <tilde-Atom>
	| <tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom> <tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
	| <tilde-Atom> <tilde-Atom> <tilde-Atom>
	| <tilde-Atom> <tilde-Atom> <tilde-Atom>
	| <tilde-Atom> <tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
	| <tilde-Atom> <tilde-Atom> <tilde-Star> <tilde-Atom>
	| <tilde-Atom> <tilde-Atom> <tilde-Star> <tilde-Atom> <tilde-Atom>
	| <tilde-Atom> <tilde-Atom> <tilde-Under> <tilde-Atom> <tilde-Atom> <tilde-Under> <tilde-Atom> <tilde-Atom> <tilde-Under> <tilde-Atom> <tilde-Atom> <tilde-Under> <tilde-Atom> <tilde-Atom> <tilde-Under>
	| <tilde-Atom> <tilde-Brace> <tilde-Atom>
	| <tilde-Atom> <tilde-Ques> <not-Tilde> <tilde-Atom>
	| <tilde-Atom> <tilde-Star> <tilde-Atom>
	| <tilde-Atom> <tilde-T>
	| <tilde-Brace>
	| <tilde-Brace> <not-Tilde> <tilde-Atom>
	| <tilde-Brace> <tilde-Atom>
	| <tilde-Bracket>
	| <tilde-Bracket> <not-Tilde> <tilde-Atom>
	| <tilde-F>
	| <tilde-I> <tilde-Angle>
	| <tilde-P>
	| <tilde-Paren>
	| <tilde-Pipe>
	| <tilde-Ques>
	| <tilde-Ques> <not-Tilde> <tilde-Atom>
	| <tilde-Slash>
	| <tilde-T>
	| <tilde-T> <tilde-T>
	| <tilde-Under> <not-Tilde> <tilde-Atom>
	| <tilde-Under> <tilde-Angle>
	}
}
