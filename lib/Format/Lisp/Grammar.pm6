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

	token four-values {
		'~' <value-comma> ** 0..3 <value>? <options>?
	}

	token tilde-Tilde {
		'~' <value>? '~'
	}

	token tilde-A {
		<four-values> <[ a A ]>
#		'~' <value-comma> ** 0..3 <value>? <options>? <[ a A ]>
	}

	token tilde-B {
		<four-values> <[ b B ]>
#		'~' <value-comma> ** 0..3 <value>? <options>? <[ b B ]>
	}

	token tilde-C {
		'~' <options>? <[ c C ]>
	}

	token tilde-D {
		<four-values> <[ d D ]>
#		'~' <value-comma> ** 0..3 <value>? <options>? <[ d D ]>
	}

	token tilde-F {
		'~' <value-comma> ** 0..4 <value>? <options>? <[ f F ]>
	}

	token tilde-I {
		'~' <value>? <options>? <[ i I ]>
	}

	token tilde-O {
		<four-values> <[ o O ]>
#		'~' <value-comma> ** 0..3 <value>? <options>? <[ o O ]>
	}

	token tilde-P {
		'~' <value>? <options>? <[ p P ]>
	}

	token tilde-R {
		'~' <value-comma> ** 0..4 <value>? <options>? <[ r R ]>
	}

	token tilde-S {
		<four-values> <[ s S ]>
#		'~' <value-comma> ** 0..3 <value>? <options>? <[ s S ]>
	}

	token tilde-T {
		'~' <value-comma> ** 0..1 <value>? <options>? <[ t T ]>
	}

	token tilde-W {
		'~' <value>? <options>? <[ w W ]>
	}

	token tilde-X {
		<four-values> <[ x X ]>
#		'~' <value-comma> ** 0..3 <value>? <options>? <[ x X ]>
	}

	token tilde-Caret {
		<four-values> '^'
#		'~' <value-comma> ** 0..3 <value>? <options>? '^'
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
		<four-values> '<'
#		'~' <value-comma> ** 0..3 <value>? <options>? '<'
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

	token tilde-Atom {
	|	<tilde-A>
	|	<tilde-Amp>
	|	<tilde-B>
	|	<tilde-C>
	|	<tilde-Caret>
	|	<tilde-D>
	|	<tilde-F>
	|	<tilde-I>
	|	<tilde-O>
	|	<tilde-P>
	|	<tilde-Percent>
	|	<tilde-Pipe>
	|	<tilde-Ques>
	|	<tilde-R>
	|	<tilde-S>
	|	<tilde-Semi>
	|	<tilde-Slash>
	|	<tilde-Star>
	|	<tilde-T>
	|	<tilde-Tilde>
	|	<tilde-Under>
	|	<tilde-W>
	|	<tilde-X>
	}

	token tilde-Angle {
	<tilde-OAngle>
		[
		||	<not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom>
		||	<not-Tilde> <tilde-Atom> <tilde-Atom> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom>
		||	<not-Tilde> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<not-Tilde> <tilde-Atom> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <tilde-Atom>
		||	<not-Tilde> <tilde-Atom>
		||	<not-Tilde>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <not-Tilde>
		||	<tilde-Atom> <tilde-Atom>
		||	<tilde-Atom>
		]?
	<tilde-CAngle>
	}

	token tilde-Brace {
	<tilde-OBrace>
		[
		||	<not-Tilde> <tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <not-Tilde>
		||	<not-Tilde>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <not-Tilde> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <not-Tilde>
		||	<tilde-Atom> <tilde-Atom>
		||	<tilde-Atom>
		]?
	<tilde-CBrace>
	}

	token tilde-Bracket {
	<tilde-OBracket>
		[
		||	<not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <not-Tilde>
		||	<not-Tilde>
		||	<tilde-Atom> <not-Tilde>
		]?
	<tilde-CBracket>
	}

	token tilde-Paren {
	<tilde-OParen>
		[
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<not-Tilde> <tilde-Atom> <not-Tilde>
		||	<not-Tilde>
		||	<tilde-Atom>
		]
	<tilde-CParen>
	}

	token TOP {
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
		||	<tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
		||	<tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
		||	<tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <not-Tilde>
		||	<tilde-Atom> <not-Tilde> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <tilde-Brace> <tilde-Atom>
		||	<tilde-Atom> <tilde-Atom>
		||	<tilde-Atom> <not-Tilde>
		||	<tilde-Atom>
		||	<not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom>
		||	<not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom>
		||	<not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom>
		||	<not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom>
		||	<not-Tilde> <tilde-Atom> <not-Tilde> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Atom> <tilde-Atom>
		||	<not-Tilde> <tilde-Atom> <not-Tilde>
		||	<not-Tilde> <tilde-Brace> <not-Tilde>
		||	<not-Tilde> <tilde-Atom>
		||	<not-Tilde> <tilde-Angle>
		||	<not-Tilde>
		||	<tilde-Angle> <tilde-Atom>
		||	<tilde-Angle>
		||	<tilde-Bracket> <not-Tilde> <tilde-Atom>
		||	<tilde-Bracket>
		||	<tilde-Brace> <not-Tilde> <tilde-Atom>
		||	<tilde-Brace> <tilde-Atom>
		||	<tilde-Brace>
		||	<tilde-Paren>
	}
}
