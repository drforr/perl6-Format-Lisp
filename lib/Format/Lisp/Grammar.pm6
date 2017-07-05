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
		<value-comma> ** 0..9 <value>? <options>? '~'
	}

	token tilde-A {
		<value-comma> ** 0..9 <value>? <options>? <[ a A ]>
	}

	token tilde-B {
		<value-comma> ** 0..9 <value>? <options>? <[ b B ]>
	}

	token tilde-C {
		<value-comma> ** 0..9 <value>? <options>? <[ c C ]>
	}

	token tilde-D {
		<value-comma> ** 0..9 <value>? <options>? <[ d D ]>
	}

	token tilde-F {
		<value-comma> ** 0..9 <value>? <options>? <[ f F ]>
	}

	token tilde-I {
		<value-comma> ** 0..9 <value>? <options>? <[ i I ]>
	}

	token tilde-O {
		<value-comma> ** 0..9 <value>? <options>? <[ o O ]>
	}

	token tilde-P {
		<value-comma> ** 0..9 <value>? <options>? <[ p P ]>
	}

	token tilde-R {
		<value-comma> ** 0..9 <value>? <options>? <[ r R ]>
	}

	token tilde-S {
		<value-comma> ** 0..9 <value>? <options>? <[ s S ]>
	}

	token tilde-T {
		<value-comma> ** 0..9 <value>? <options>? <[ t T ]>
	}

	token tilde-W {
		<value-comma> ** 0..9 <value>? <options>? <[ w W ]>
	}

	token tilde-X {
		<value-comma> ** 0..9 <value>? <options>? <[ x X ]>
	}

	token tilde-Caret {
		<value-comma> ** 0..9 <value>? <options>? '^'
	}

	token tilde-Ques {
		<value-comma> ** 0..9 <value>? <options>? '?'
	}

	token tilde-Star {
		<value-comma> ** 0..9 <value>? <options>? '*'
	}

	token tilde-OParen {
		'~' <value-comma> ** 0..9 <value>? <options>? '('
	}

	token tilde-CParen {
		'~' <value-comma> ** 0..9 <value>? <options>? ')'
	}

	token tilde-OBrace {
		'~' <value-comma> ** 0..9 <value>? <options>? '{'
	}

	token tilde-CBrace {
		'~' <value-comma> ** 0..9 <value>? <options>? '}'
	}

	token tilde-OBracket {
		'~' <value-comma> ** 0..9 <value>? <options>? '['
	}

	token tilde-Semi {
		<value-comma> ** 0..9 <value>? <options>? ';'
	}

	token tilde-CBracket {
		'~' <value-comma> ** 0..9 <value>? <options>? ']'
	}

	token tilde-OAngle {
		'~' <value-comma> ** 0..9 <value>? <options>? '<'
	}

	token tilde-CAngle {
		'~' <value-comma> ** 0..9 <value>? <options>? '>'
	}

	token tilde-Percent {
		<value-comma> ** 0..9 <value>? <options>? '%'
	}

	token tilde-Pipe {
		<value-comma> ** 0..9 <value>? <options>? '|'
	}

	token tilde-Amp {
		<value-comma> ** 0..9 <value>? <options>? '&'
	}

	token tilde-Under {
		<value-comma> ** 0..9 <value>? <options>? '_'
	}

	token tilde-Slash {
		<value-comma> ** 0..9 <value>? <options>? '/' <-[ / ]>+ '/'
	}

	token Atom {
	|	'~'
		[
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
		]
	|	<not-Tilde>
	}

	token tilde-Angle {
		<tilde-OAngle> <TOP>? <tilde-CAngle>
	}

	token tilde-Brace {
		<tilde-OBrace> <TOP>? <tilde-CBrace>
	}

	token tilde-Bracket {
		<tilde-OBracket> <TOP>? <tilde-CBracket>
	}

	token tilde-Paren {
		<tilde-OParen> <TOP>? <tilde-CParen>
	}

	token Non-Terminal {
	||	<tilde-Angle>
	||	<tilde-Brace>
	||	<tilde-Bracket>
	||	<tilde-Paren>
	}

	token Term {
		<Non-Terminal> <Atom>*
	}

	token TOP {
		||	<Atom>+ <Term>*
		||	<Term>+
	}
}
