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

	token tilde-Tilde {
	'~'	[
		|	<value>
		]?
	'~'
	}

	token options {
	|	'@' ':'?
	|	':' '@'?
	}

	token tilde-A {
	'~'	[
		|	<options>
		|	<value> ',' ',' ',' '@'?
		|	<value> ',' ',' ',' <value> '@'?
		|	<value> ',' ',' <value> '@'?
		|	<value> ',' <value> '@'?
		|	<value> ':' '@'
		|	<value> '@' ':'
		|	<value> <[ @ : ]>?
		]?
	<[ a A ]>
	}

	token tilde-B {
	'~'	[
		|	',' ',' ',' <value> ':'
		|	',' ',' ',' <value> '@' ':'
		|	',' ',' <value> ',' <value> ':' '@'?
		|	',' ',' <value> ',' <value> '@' ':'
		|	',' ',' <value> ':'
		|	<options>
		|	<value> ',' <value>
		|	<value> ',' <value> ',' <value> ',' <value>
		|	<value> '@'?
		]?
	<[ b B ]>
	}

	token tilde-C {
	'~'	[
		|	<options>
		]?
	<[ c C ]>
	}

	token tilde-D {
	'~'	[
		|	',' ',' ',' <value> ':' '@'?
		|	',' ',' ',' <value> '@' ':'
		|	',' ',' <value> ',' <value> ':' '@'?
		|	',' ',' <value> ':'
		|	<options>
		|	<value> ',' <value> ',' <value> ',' <value>
		|	<value> ',' <value> '@'?
		|	<value> '@'?
		]?
	<[ d D ]>
	}

	token tilde-F {
	'~'	[
		|	',' ',' ',' ',' <value>
		|	',' ',' ',' <value>
		|	',' ',' <value>
		|	',' <value>
		|	<value> ',' <value> ',' ','
		|	<value> ',' <value> ',' ',' ',' <value>
		|	<value> ',' <value> ',' ',' <value>
		|	<value> ',' <value> ',' <value>
		|	<value> ',' <value> ',' <value> ',' <value> ',' <value>
		|	<value> ',' <value> '@'?
		|	<value> '@'?
		]?
	<[ f F ]>
	}

	token tilde-I {
	'~'	[
		|	<options>
		|	<value> ':'?
		]?
	<[ i I ]>
	}

	token tilde-O {
	'~'	[
		|	',' ',' ',' <value> ':' '@'?
		|	',' ',' ',' <value> '@' ':'
		|	',' ',' <value> ',' <value> ':' '@'?
		|	',' ',' <value> ',' <value> '@' ':'
		|	',' ',' <value> ':'
		|	<options>
		|	<value> ',' <value> ',' <value> ',' <value>
		|	<value> ',' <value> '@'?
		|	<value> '@'?
		]?
	<[ o O ]>
	}

	token tilde-P {
	'~'	[
		|	<options>
		]?
	<[ p P ]>
	}

	token tilde-R {
	'~'	[
		|	<options>
		|	<value> ',' ',' ',' ',' <value> ':'?
		|	<value> ',' ',' ',' <value> ':'
		|	<value> ',' <value>
		|	<value> ',' <value> ',' ',' <value> ':'
		|	<value> ',' <value> ',' <value>
		|	<value> ',' <value> ',' <value> ',' <value> ',' <value>
		|	<value> ',' <value> ',' <value> ',' <value> ':'
		|	<value> ',' <value> ':' '@'
		|	<value> '@' ':'
		|	<value> <[ @ : ]>?
		]?
	<[ r R ]>
	}

	token tilde-S {
	'~'	[
		|	<options>
		|	<value> ',' ',' ',' '@'?
		|	<value> ',' ',' ',' <value> '@'?
		|	<value> ',' ',' <value> '@'?
		|	<value> ',' <value> '@'?
		|	<value> ':' '@'
		|	<value> '@' ':'
		|	<value> <[ @ : ]>?
		]?
	<[ s S ]>
	}

	token tilde-T {
	'~'	[
		|	',' <value> ':' '@'?
		|	<options>
		|	<value> ',' ':' '@'?
		|	<value> ',' <value> ':' '@'
		|	<value> ',' <value> '@' ':'
		|	<value> ',' <value> <[ @ : ]>?
		|	<value> ':' '@'
		|	<value> ':'?
		]?
	<[ t T ]>
	}

	token tilde-W {
	'~'	#[
		#]?
	<[ w W ]>
	}

	token tilde-X {
	'~'	[
		|	',' ',' ',' <value> ':'
		|	',' ',' ',' <value> '@' ':'
		|	',' ',' <value> ',' <value> ':' '@'?
		|	',' ',' <value> ':'
		|	<options>
		|	<value> ',' <value> ',' <value> ',' <value>
		|	<value> ',' <value> '@'?
		|	<value> '@'?
		]?
	<[ x X ]>
	}

	token tilde-Caret {
	'~'	[
		|	<options>
		|	<value> ',' <value> ',' <value> ',' <value> ':'?
		|	<value> ',' <value> ',' <value> ':'?
		|	<value> ',' <value> ':'?
		|	<value> ':'?
		]?
	'^'
	}

	token tilde-Ques {
	'~'	[
		|	'@'
		]?
	'?'
	}

	token tilde-Star {
	'~'	[
		|	<options>
		|	<value> <[ @ : ]>?
		]?
	'*'
	}

	token tilde-OParen {
	'~'	[
		|	<options>
		|	<value>
		]?
	'('
	}

	token tilde-CParen {
	'~'	[
		|	<options>
		|	<value>
		]?
	')'
	}

	token tilde-OBrace {
	'~'	[
		|	<options>
		|	<value> ':' '@'
		|	<value> '@' ':'
		|	<value> <[ @ : ]>?
		]?
	'{'
	}

	token tilde-CBrace {
	'~'	[
		|	':'
		]?
	'}'
	}

	token tilde-OBracket {
	'~'	[
		|	<options>
		|	<value>
		]?
	'['
	}

	token tilde-Semi {
	'~'	[
		|	<options>
		|	<value> ',' <value> ':'
		]?
	';'
	}

	token tilde-CBracket {
	|	'~]'
	}

	token tilde-OAngle {
	'~'	[
		|	',' ',' <value> ',' <value>
		|	',' ',' <value> ','?
		|	',' <value>
		|	<options>
		|	<value> ',' ',' ',' <value>
		|	<value> ',' ',' <value>
		|	<value> ':' '@'
		|	<value> <[ @ : ]>?
		]?
	'<'
	}

	token tilde-CAngle {
	'~'	[
		|	<options>
		]?
	'>'
	}

	token tilde-Percent {
	'~'	[
		|	<value>
		]?
	'%'
	}

	token tilde-Pipe {
	'~'	[
		|	<value>
		]?
	'|'
	}

	token tilde-Amp {
	'~'	[
		|	<value>
		]?
	'&'
	}

	token tilde-Comma {
	'~'	#[
		#]
	','
	}

	token tilde-Under {
	'~'	[
		|	<options>
		]?
	'_'
	}

	token tilde-Slash {
	'~'	[
		|	<options>
		|	<value> ',' <value> ',' <value> ',' <value> ',' <value> ',' <value> ',' <value> ',' <value> ',' <value> ',' <value> '@'
		|	<value> '@' ':'
		|	<value> <[ @ : ]>?
		]?
	'/' <-[ / ]>+ '/'
	}

	token tilde-Angle {
	<tilde-OAngle>
		[
		| <not-Tilde>
		| <not-Tilde> <tilde-A> <tilde-Semi> <tilde-A>
		| <not-Tilde> <tilde-A> <tilde-Semi> <tilde-A> <tilde-Comma> <not-Tilde>
		| <not-Tilde> <tilde-A> <tilde-Semi> <tilde-A> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Angle> <not-Tilde>
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
		| <not-Tilde> <tilde-Semi> <tilde-A> <tilde-Semi> <not-Tilde> <tilde-A>
		| <not-Tilde> <tilde-Semi> <tilde-Brace>
		| <not-Tilde> <tilde-Semi> <tilde-Brace> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-Semi> <tilde-T>
		| <not-Tilde> <tilde-Semi> <tilde-T> <tilde-Semi> <not-Tilde>
		| <not-Tilde> <tilde-T> <not-Tilde>
		| <not-Tilde> <tilde-Under> <not-Tilde>
		| <not-Tilde> <tilde-W> <not-Tilde>
		| <tilde-A>
		| <tilde-A> <tilde-Caret> <not-Tilde>
		| <tilde-A> <tilde-Semi> <tilde-A>
		| <tilde-A> <tilde-T>
		| <tilde-Angle>
		| <tilde-Angle> <not-Tilde>
		| <tilde-Brace>
		| <tilde-Percent> <not-Tilde> <tilde-Semi> <not-Tilde>
		| <tilde-Percent> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde> <tilde-Semi> <not-Tilde>
		| <tilde-Semi>
		| <tilde-Semi> <tilde-A>
		| <tilde-Semi> <tilde-A> <tilde-Semi>
		| <tilde-Semi> <tilde-A> <tilde-Semi> <not-Tilde> <tilde-A>
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
		| <not-Tilde> <tilde-A> <not-Tilde>
		| <not-Tilde> <tilde-A> <not-Tilde> <tilde-A> <not-Tilde>
		| <not-Tilde> <tilde-A> <tilde-Caret> <not-Tilde> <tilde-A> <tilde-Caret> <not-Tilde>
		| <tilde-A>
		| <tilde-A> <not-Tilde> <tilde-A>
		| <tilde-A> <tilde-A> <tilde-A> <tilde-A> <tilde-Star> <tilde-Caret> <tilde-A>
		| <tilde-A> <tilde-A> <tilde-A> <tilde-A> <tilde-Star> <tilde-Caret> <tilde-A> <tilde-A> <tilde-A> <tilde-A>
		| <tilde-A> <tilde-A> <tilde-A> <tilde-Star> <tilde-A>
		| <tilde-A> <tilde-A> <tilde-A> <tilde-Star> <tilde-A> <tilde-A>
		| <tilde-A> <tilde-A> <tilde-A> <tilde-Star> <tilde-A> <tilde-A> <tilde-A> <tilde-A>
		| <tilde-A> <tilde-A> <tilde-Caret> <tilde-A>
		| <tilde-A> <tilde-A> <tilde-Star> <tilde-A>
		| <tilde-A> <tilde-A> <tilde-Star> <tilde-A> <tilde-A>
		| <tilde-A> <tilde-Caret>
		| <tilde-A> <tilde-Caret> <not-Tilde>
		| <tilde-A> <tilde-Caret> <not-Tilde> <tilde-Under>
		| <tilde-A> <tilde-Caret> <tilde-A>
		| <tilde-A> <tilde-Caret> <tilde-A> <tilde-A>
		| <tilde-A> <tilde-Ques> <tilde-A>
		| <tilde-A> <tilde-Star> <tilde-A>
		| <tilde-Brace>
		| <tilde-Bracket>
		| <tilde-Caret> <tilde-A>
		| <tilde-Caret> <tilde-A> <tilde-Caret> <tilde-A> <tilde-Caret> <tilde-A> <tilde-Caret> <tilde-A>
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
		| <not-Tilde> <tilde-A> <not-Tilde>
		| <not-Tilde> <tilde-Paren> <not-Tilde>
		| <tilde-C>
		| <tilde-C> <not-Tilde> <tilde-C> <not-Tilde> <tilde-Caret> <tilde-C>
		| <tilde-C> <tilde-C> <tilde-Caret> <tilde-C>
		]
	<tilde-CParen>
	}

	token TOP {
	| <not-Tilde>
	| <not-Tilde> <tilde-A> <not-Tilde>
	| <not-Tilde> <tilde-A> <not-Tilde> <tilde-A> <not-Tilde>
	| <not-Tilde> <tilde-Amp>
	| <not-Tilde> <tilde-Angle>
	| <not-Tilde> <tilde-Angle> <not-Tilde>
	| <not-Tilde> <tilde-Brace> <not-Tilde>
	| <not-Tilde> <tilde-C>
	| <not-Tilde> <tilde-C> <not-Tilde>
	| <not-Tilde> <tilde-Percent>
	| <not-Tilde> <tilde-Percent> <tilde-Amp>
	| <not-Tilde> <tilde-Ques> <not-Tilde>
	| <not-Tilde> <tilde-T>
	| <not-Tilde> <tilde-T> <not-Tilde>
	| <not-Tilde> <tilde-T> <not-Tilde> <tilde-Angle> <not-Tilde>
	| <not-Tilde> <tilde-Tilde> <tilde-D> <not-Tilde>
	| <not-Tilde> <tilde-Under>
	| <not-Tilde> <tilde-Under> <not-Tilde>
	| <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under>
	| <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under>
	| <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under>
	| <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <tilde-Percent> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under> <not-Tilde> <tilde-Under>
	| <tilde-A>
	| <tilde-A> <not-Tilde> <tilde-Ques> <not-Tilde> <tilde-A>
	| <tilde-A> <tilde-A> <tilde-Star> <tilde-A>
	| <tilde-A> <tilde-A> <tilde-Star> <tilde-A> <tilde-A>
	| <tilde-A> <tilde-Angle>
	| <tilde-A> <tilde-Brace> <tilde-A>
	| <tilde-A> <tilde-Ques> <not-Tilde> <tilde-A>
	| <tilde-A> <tilde-Star> <tilde-A>
	| <tilde-A> <tilde-T>
	| <tilde-A> <tilde-Tilde> <not-Tilde> <tilde-A> <tilde-Tilde> <tilde-D> <not-Tilde> <tilde-D> <not-Tilde> <tilde-Tilde> <not-Tilde>
	| <tilde-A> <tilde-Tilde> <tilde-D> <not-Tilde> <tilde-D> <not-Tilde>
	| <tilde-Amp>
	| <tilde-Angle>
	| <tilde-Angle> <not-Tilde> <tilde-T> <not-Tilde>
	| <tilde-Angle> <tilde-Angle>
	| <tilde-Angle> <tilde-I>
	| <tilde-Angle> <tilde-Under>
	| <tilde-Angle> <tilde-W>
	| <tilde-B>
	| <tilde-Brace>
	| <tilde-Brace> <not-Tilde> <tilde-A>
	| <tilde-Brace> <tilde-A>
	| <tilde-Bracket>
	| <tilde-Bracket> <not-Tilde> <tilde-A>
	| <tilde-C>
	| <tilde-D>
	| <tilde-D> <not-Tilde>
	| <tilde-D> <not-Tilde> <tilde-P>
	| <tilde-F>
	| <tilde-I> <tilde-Angle>
	| <tilde-O>
	| <tilde-P>
	| <tilde-Paren>
	| <tilde-Percent>
	| <tilde-Percent> <not-Tilde> <tilde-Angle>
	| <tilde-Percent> <not-Tilde> <tilde-Angle> <not-Tilde> <tilde-Angle> <not-Tilde> <tilde-Angle>
	| <tilde-Percent> <not-Tilde> <tilde-Angle> <tilde-Angle> <tilde-Angle>
	| <tilde-Percent> <not-Tilde> <tilde-Under>
	| <tilde-Pipe>
	| <tilde-Ques>
	| <tilde-Ques> <not-Tilde> <tilde-A>
	| <tilde-R>
	| <tilde-S>
	| <tilde-Slash>
	| <tilde-T>
	| <tilde-T> <tilde-T>
	| <tilde-Tilde>
	| <tilde-Tilde> <not-Tilde> <tilde-C> <not-Tilde>
	| <tilde-Tilde> <not-Tilde> <tilde-C> <not-Tilde> <tilde-C>
	| <tilde-Tilde> <tilde-D> <not-Tilde>
	| <tilde-Tilde> <tilde-D> <not-Tilde> <tilde-C>
	| <tilde-Tilde> <tilde-D> <not-Tilde> <tilde-C> <not-Tilde>
	| <tilde-Tilde> <tilde-D> <not-Tilde> <tilde-C> <not-Tilde> <tilde-Tilde> <not-Tilde> <tilde-Tilde> <not-Tilde>
	| <tilde-Tilde> <tilde-D> <not-Tilde> <tilde-C> <tilde-C>
	| <tilde-Tilde> <tilde-D> <not-Tilde> <tilde-D> <not-Tilde>
	| <tilde-Tilde> <tilde-D> <tilde-C>
	| <tilde-Tilde> <tilde-D> <tilde-Tilde>
	| <tilde-Tilde> <tilde-R>
	| <tilde-Under> <not-Tilde> <tilde-Percent>
	| <tilde-Under> <tilde-Angle>
	| <tilde-W> <tilde-Angle>
	| <tilde-W> <tilde-W> <tilde-Under> <tilde-W> <tilde-W> <tilde-Under> <tilde-W> <tilde-W> <tilde-Under> <tilde-W> <tilde-W> <tilde-Under> <tilde-W> <tilde-W> <tilde-Under>
	| <tilde-X>
	}
}
