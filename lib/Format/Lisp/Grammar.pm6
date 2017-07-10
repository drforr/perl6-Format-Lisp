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
	token At { <[ @ ]> }
	token Colon { <[ : ]> }

	token not-Tilde { <-[ ~ ]>+ }

	token value {
	|	'#'
	|	'\'' <[ a..z A..Z , * ]>
	|	<V>
	|	<signed-integer>
	}

	token options {
	|	<At> <Colon>
	|	<At>
	|	<Colon> <At>
	|	<Colon>
	}

	token value-comma {
		<value>? ','
	}

	token Tilde-Options {
		<value-comma> ** 0..9 <value>? <options>?
	}

	token tilde-Tilde { '~' }

	token tilde-OAngle { <Tilde-Options> '<' }
	token tilde-CAngle { <Tilde-Options> '>' }

	token tilde-A { <[ a A ]> }

	token tilde-OBrace { <Tilde-Options> '{' }
	token tilde-CBrace { <Tilde-Options> '}' }

	token tilde-OBracket { <Tilde-Options> '[' }
	token tilde-CBracket { <Tilde-Options> ']' }

	token tilde-B { <[ b B ]> }

	token tilde-Caret { '^' }

	token tilde-C { <[ c C ]> }

	token tilde-D { <[ d D ]> }

	token tilde-F { <[ f F ]> }

	token tilde-I { <[ i I ]> }

	token tilde-O { <[ o O ]> }

	token tilde-OParen { <Tilde-Options> '(' }
	token tilde-CParen { <Tilde-Options> ')' }

	token tilde-P { <[ p P ]> }

	token tilde-Ques { '?' }

	token tilde-R { <[ r R ]> }

	token tilde-Star { '*' }

	token tilde-S { <[ s S ]> }

	token tilde-T { <[ t T ]> }

	token tilde-W { <[ w W ]> }

	token tilde-X { <[ x X ]> }

	token tilde-Semi { ';' }

	token tilde-Percent { '%' }

	token tilde-Pipe { '|' }

	token tilde-Amp { '&' }

	token tilde-Under { '_' }

	token tilde-Slash { '/' ( <-[ / ]>+ ) '/' }

	token Atom {
	|	'~' <Tilde-Options>
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

	token tilde-Angle { <tilde-OAngle> <TOP>? '~' <tilde-CAngle> }

	token tilde-Brace { <tilde-OBrace> <TOP>? '~' <tilde-CBrace> }

	token tilde-Bracket { <tilde-OBracket> <TOP>? '~' <tilde-CBracket> }

	token tilde-Paren { <tilde-OParen> <TOP>? '~' <tilde-CParen> }

	token Non-Terminal {
	'~'
		[
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
