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

	token tilde-Tilde { '~' }

	token tilde-A { <[ a A ]> }

	token tilde-B { <[ b B ]> }

	token tilde-C { <[ c C ]> }

	token tilde-D { <[ d D ]> }

	token tilde-F { <[ f F ]> }

	token tilde-I { <[ i I ]> }

	token tilde-O { <[ o O ]> }

	token tilde-P { <[ p P ]> }

	token tilde-R { <[ r R ]> }

	token tilde-S { <[ s S ]> }

	token tilde-T { <[ t T ]> }

	token tilde-W { <[ w W ]> }

	token tilde-X { <[ x X ]> }

	token tilde-Caret { '^' }

	token tilde-Ques { '?' }

	token tilde-Star { '*' }

	token tilde-OParen {
		<value-comma> ** 0..9 <value>? <options>? '('
	}

	token tilde-CParen {
		<value-comma> ** 0..9 <value>? <options>? ')'
	}

	token tilde-OBrace {
		<value-comma> ** 0..9 <value>? <options>? '{'
	}

	token tilde-CBrace {
		<value-comma> ** 0..9 <value>? <options>? '}'
	}

	token tilde-OBracket {
		<value-comma> ** 0..9 <value>? <options>? '['
	}

	token tilde-Semi { ';' }

	token tilde-CBracket {
		<value-comma> ** 0..9 <value>? <options>? ']'
	}

	token tilde-OAngle {
		<value-comma> ** 0..9 <value>? <options>? '<'
	}

	token tilde-CAngle {
		<value-comma> ** 0..9 <value>? <options>? '>'
	}

	token tilde-Percent { '%' }

	token tilde-Pipe { '|' }

	token tilde-Amp { '&' }

	token tilde-Under { '_' }

	token tilde-Slash { '/' <-[ / ]>+ '/' }

	token Atom {
	|	'~' <value-comma> ** 0..9 <value>? <options>?
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
		<tilde-OAngle> <TOP>? '~' <tilde-CAngle>
	}

	token tilde-Brace {
		<tilde-OBrace> <TOP>? '~' <tilde-CBrace>
	}

	token tilde-Bracket {
		<tilde-OBracket> <TOP>? '~' <tilde-CBracket>
	}

	token tilde-Paren {
		<tilde-OParen> <TOP>? '~' <tilde-CParen>
	}

	token Non-Terminal {
	'~'	[
		||	<tilde-Angle>
		||	<tilde-Brace>
		||	<tilde-Bracket>
		||	<tilde-Paren>
		]
	}

	token Term {
		<Non-Terminal> <Atom>*
	}

	token TOP {
	||	<Atom>+ <Term>*
	||	<Term>+
	}
}
