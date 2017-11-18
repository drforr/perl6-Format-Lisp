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

	token Default { '#' }

	token Character {
	|	<[ a..z A..Z , * ]>
	}

	token V { <[ v V ]> }
	token At { <[ @ ]> }
	token Colon { <[ : ]> }

	token not-Tilde { <-[ ~ ]>+ }

	token value {
	|	<Default>
	|	'\'' <Character>
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

	token tilde-OAngle { '<' }
	token tilde-CAngle { <Tilde-Options> '>' }

	token tilde-A { <[ a A ]> }

	token tilde-OBrace { '{' }
	token tilde-CBrace { <Tilde-Options> '}' }

	token tilde-OBracket { '[' }
	token tilde-CBracket { <Tilde-Options> ']' }

	token tilde-B { <[ b B ]> }

	token tilde-Caret { '^' }

	token tilde-C { <[ c C ]> }

	token tilde-D { <[ d D ]> }

	token tilde-Dollar { '$' }

	token tilde-E { <[ e E ]> }

	token tilde-F { <[ f F ]> }

	token tilde-G { <[ g G ]> }

	token tilde-I { <[ i I ]> }

	token tilde-Newline { "\n" }

	token tilde-O { <[ o O ]> }

	token tilde-OParen { '(' }
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

	token tilde-Unused {
		<[ H h J j K k L l M m N n Q q U u V v Y y Z z ` ]>
	}

	token Atom {
	|	'~' <Tilde-Options>
		[
		|	<tilde-A>
		|	<tilde-Amp>
		|	<tilde-Angle>
		|	<tilde-B>
		|	<tilde-Brace>
		|	<tilde-Bracket>
		|	<tilde-C>
		|	<tilde-Caret>
		|	<tilde-D>
		|	<tilde-Dollar>
		|	<tilde-E>
		|	<tilde-F>
		|	<tilde-G>
		|	<tilde-I>
		|	<tilde-Newline>
		|	<tilde-O>
		|	<tilde-P>
		|	<tilde-Paren>
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
		|	<tilde-Unused>
		]
	|	<not-Tilde>
	}

	token tilde-Angle { <tilde-OAngle> <TOP>? '~' <tilde-CAngle> }

	token tilde-Brace { <tilde-OBrace> <TOP>? '~' <tilde-CBrace> }

	token tilde-Bracket { <tilde-OBracket> <TOP>? '~' <tilde-CBracket> }

	token tilde-Paren { <tilde-OParen> <TOP>? '~' <tilde-CParen> }

	token TOP { <Atom>+ }
}
