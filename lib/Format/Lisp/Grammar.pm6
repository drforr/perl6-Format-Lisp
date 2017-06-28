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

	token not-Tilde { <-[ ~ ]> }

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

	token tilde-A {
	'~'	[
		|	':'
		|	'@'
		|	<value> ',' <value> '@'?
		|	<value> ',,' <value> '@'?
		|	<value> ',,,' '@'?
		|	<value> ',,,' <value> '@'?
		|	<value> ':' '@'
		|	<value> '@' ':'
		|	<value> <[ @ : ]>?
		]?
	<[ a A ]>
	}

	token tilde-B {
	'~'	[
		|	',,' <value> ',' <value> ':' '@'?
		|	',,' <value> ',' <value> '@' ':'
		|	',,' <value> ':'
		|	',,,' <value> ':'
		|	',,,' <value> '@' ':'
		|	':'
		|	':' '@'
		|	'@'
		|	'@' ':'
		|	<value> ',' <value>
		|	<value> ',' <value> ',' <value> ',' <value>
		|	<value> '@'?
		]?
	<[ b B ]>
	}

	token tilde-C {
	'~'	[
		|	':' '@'?
		|	'@'
		|	'@' ':'
		]?
	<[ c C ]>
	}

	token tilde-D {
	'~'	[
		|	',,' <value> ',' <value> ':' '@'?
		|	',,' <value> ':'
		|	',,,' <value> ':' '@'?
		|	',,,' <value> '@' ':'
		|	':'
		|	'@'
		|	'@' ':'
		|	<value> ',' <value> ',' <value> ',' <value>
		|	<value> ',' <value> '@'?
		|	<value> '@'?
		]?
	<[ d D ]>
	}

	token tilde-F {
	'~'	[
		|	',' <value>
		|	',,' <value>
		|	',,,' <value>
		|	',,,,' <value>
		|	<value> ',' <value> ',' <value>
		|	<value> ',' <value> ',' <value> ',' <value> ',' <value>
		|	<value> ',' <value> ',,'
		|	<value> ',' <value> ',,' <value>
		|	<value> ',' <value> ',,,' <value>
		|	<value> ',' <value> '@'?
		|	<value> '@'?
		]?
	<[ f F ]>
	}

	token tilde-I {
	'~'	[
		|	':'
		|	<value> ':'?
		]?
	<[ i I ]>
	}

	token tilde-O {
	'~'	[
		|	',,' <value> ',' <value> ':' '@'?
		|	',,' <value> ',' <value> '@' ':'
		|	',,' <value> ':'
		|	',,,' <value> ':' '@'?
		|	',,,' <value> '@' ':'
		|	':' '@'?
		|	'@' ':'?
		|	<value> ',' <value> ',' <value> ',' <value>
		|	<value> ',' <value> '@'?
		|	<value> '@'?
		]?
	<[ o O ]>
	}

	token tilde-P {
	'~'	[
		|	':' '@'?
		|	'@' ':'?
		]?
	<[ p P ]>
	}

	token tilde-R {
	'~'	[
		|	':' '@'?
		|	'@' ':'?
		|	<value> ',' <value>
		|	<value> ',' <value> ',' <value>
		|	<value> ',' <value> ',' <value> ',' <value> ',' <value>
		|	<value> ',' <value> ',' <value> ',' <value> ':'
		|	<value> ',' <value> ',,' <value> ':'
		|	<value> ',' <value> ':' '@'
		|	<value> ',,,' <value> ':'
		|	<value> ',,,,' <value> ':'?
		|	<value> '@' ':'
		|	<value> <[ @ : ]>?
		]?
	<[ r R ]>
	}

	token tilde-S {
	'~'	[
		|	':'
		|	'@'
		|	<value> ',' <value> '@'?
		|	<value> ',,' <value> '@'?
		|	<value> ',,,' '@'?
		|	<value> ',,,' <value> '@'?
		|	<value> ':' '@'
		|	<value> '@' ':'
		|	<value> <[ @ : ]>?
		]?
	<[ s S ]>
	}

	token tilde-T {
	'~'	[
		|	',' <value> ':' '@'?
		|	':' '@'
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
		|	',,' <value> ',' <value> ':' '@'?
		|	',,' <value> ':'
		|	',,,' <value> ':'
		|	',,,' <value> '@' ':'
		|	':' '@'?
		|	'@' ':'?
		|	<value> ',' <value> ',' <value> ',' <value>
		|	<value> ',' <value> '@'?
		|	<value> '@'?
		]?
	<[ x X ]>
	}

	token tilde-Caret {
	'~'	[
		|	':'
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
		|	':'
		|	'@'
		|	<value> <[ @ : ]>?
		]?
	'*'
	}

	token tilde-OParen {
	'~'	[
		|	':' '@'?
		|	'@' ':'?
		|	<value>
		]?
	'('
	}

	token tilde-CParen {
	'~'	[
		|	':' '@'?
		|	'@'
		|	<value>
		]?
	')'
	}

	token tilde-OBrace {
	'~'	[
		|	':' '@'?
		|	'@' ':'?
		|	<value> <[ @ : ]>?
		|	<value> ':' '@'
		|	<value> '@' ':'
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
		|	<value>
		|	'@'
		|	':'
		]?
	'['
	}

	token tilde-Semi {
	'~'	[
		|	':'
		|	'@'
		|	<value> ',' <value> ':'
		]?
	';'
	}

	token tilde-CBracket {
	|	'~]'
	}

	token tilde-OAngle {
	'~'	[
		|	',' <value>
		|	',,' <value> ',' <value>
		|	',,' <value> ','?
		|	':' '@'?
		|	'@' ':'?
		|	<value> ',,' <value>
		|	<value> ',,,' <value>
		|	<value> ':' '@'
		|	<value> <[ @ : ]>?
		]?
	'<'
	}

	token tilde-CAngle {
	'~'	[
		|	'@'
		|	':' '@'?
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
		|	':' '@'?
		|	'@' ':'?
		]?
	'_'
	}

	token tilde-OSlash {
	'~'	[
		|	':' '@'?
		|	'@' ':'?
		|	<value> ',' <value> ',' <value> ',' <value> ',' <value> ',' <value> ',' <value> ',' <value> ',' <value> ',' <value> '@'
		|	<value> <[ @ : ]>?
		|	<value> '@' ':'
		]?
	'/'
	}

	token tilde-CSlash {
		'/'
	}

	token TOP {
	| <not-Tilde>+
	| <not-Tilde>+ <tilde-A> <not-Tilde>+
	| <not-Tilde>+ <tilde-A> <not-Tilde>+ <tilde-A> <not-Tilde>+
	| <not-Tilde>+ <tilde-Amp>
	| <not-Tilde>+ <tilde-C>
	| <not-Tilde>+ <tilde-C> <not-Tilde>+
	| <not-Tilde>+ <tilde-OAngle> <not-Tilde>+ <tilde-I> <tilde-Under> <not-Tilde>+ <tilde-CAngle>
	| <not-Tilde>+ <tilde-OAngle> <not-Tilde>+ <tilde-OAngle> <not-Tilde>+ <tilde-CAngle> <not-Tilde>+ <tilde-CAngle> <not-Tilde>+
	| <not-Tilde>+ <tilde-OBrace> <tilde-A> <tilde-Caret> <not-Tilde>+ <tilde-CBrace> <not-Tilde>+
	| <not-Tilde>+ <tilde-Percent>
	| <not-Tilde>+ <tilde-Percent> <tilde-Amp>
	| <not-Tilde>+ <tilde-Ques> <not-Tilde>+
	| <not-Tilde>+ <tilde-T>
	| <not-Tilde>+ <tilde-T> <not-Tilde>+
	| <not-Tilde>+ <tilde-T> <not-Tilde>+ <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-CAngle> <not-Tilde>+
	| <not-Tilde>+ <tilde-Tilde> <tilde-D> <not-Tilde>+
	| <not-Tilde>+ <tilde-Under>
	| <not-Tilde>+ <tilde-Under> <not-Tilde>+
	| <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under>
	| <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under>
	| <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under>
	| <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <tilde-Percent> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-Under>
	| <tilde-A>
	| <tilde-A> <not-Tilde>+ <tilde-Ques> <not-Tilde>+ <tilde-A>
	| <tilde-A> <tilde-A> <tilde-Star> <tilde-A>
	| <tilde-A> <tilde-A> <tilde-Star> <tilde-A> <tilde-A>
	| <tilde-A> <tilde-OAngle> <tilde-A> <tilde-T> <tilde-CAngle>
	| <tilde-A> <tilde-OAngle> <tilde-T> <tilde-CAngle>
	| <tilde-A> <tilde-OBrace> <tilde-A> <tilde-A> <tilde-A> <tilde-A> <tilde-Star> <tilde-Caret> <tilde-A> <tilde-A> <tilde-A> <tilde-A> <tilde-CBrace> <tilde-A>
	| <tilde-A> <tilde-OBrace> <tilde-A> <tilde-A> <tilde-A> <tilde-A> <tilde-Star> <tilde-Caret> <tilde-A> <tilde-CBrace> <tilde-A>
	| <tilde-A> <tilde-OBrace> <tilde-A> <tilde-A> <tilde-A> <tilde-Star> <tilde-A> <tilde-A> <tilde-A> <tilde-A> <tilde-CBrace> <tilde-A>
	| <tilde-A> <tilde-OBrace> <tilde-A> <tilde-A> <tilde-A> <tilde-Star> <tilde-A> <tilde-A> <tilde-CBrace> <tilde-A>
	| <tilde-A> <tilde-OBrace> <tilde-A> <tilde-A> <tilde-A> <tilde-Star> <tilde-A> <tilde-CBrace> <tilde-A>
	| <tilde-A> <tilde-OBrace> <tilde-A> <tilde-A> <tilde-Star> <tilde-A> <tilde-A> <tilde-CBrace> <tilde-A>
	| <tilde-A> <tilde-OBrace> <tilde-A> <tilde-A> <tilde-Star> <tilde-A> <tilde-CBrace> <tilde-A>
	| <tilde-A> <tilde-OBrace> <tilde-A> <tilde-Star> <tilde-A> <tilde-CBrace> <tilde-A>
	| <tilde-A> <tilde-Ques> <not-Tilde>+ <tilde-A>
	| <tilde-A> <tilde-Star> <tilde-A>
	| <tilde-A> <tilde-T>
	| <tilde-A> <tilde-Tilde> <not-Tilde>+ <tilde-A> <tilde-Tilde> <tilde-D> <not-Tilde>+ <tilde-D> <not-Tilde>+ <tilde-Tilde> <not-Tilde>+
	| <tilde-A> <tilde-Tilde> <tilde-D> <not-Tilde>+ <tilde-D> <not-Tilde>+
	| <tilde-Amp>
	| <tilde-B>
	| <tilde-C>
	| <tilde-D>
	| <tilde-D> <not-Tilde>+
	| <tilde-D> <not-Tilde>+ <tilde-P>
	| <tilde-F>
	| <tilde-I> <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-CAngle>
	| <tilde-O>
	| <tilde-OAngle> <not-Tilde>+ <tilde-A> <tilde-Semi> <tilde-A> <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-A> <tilde-Semi> <tilde-A> <tilde-Comma> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-A> <tilde-Semi> <tilde-A> <tilde-Semi> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Caret> <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-I> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-I> <tilde-Under> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-I> <tilde-Under> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-CAngle> <not-Tilde>+ <tilde-T> <not-Tilde>+
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-CAngle> <tilde-I>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-CAngle> <tilde-Under>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-CAngle> <tilde-W>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-Caret> <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-Caret> <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-Caret> <tilde-Semi> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-I> <tilde-Under> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-T> <tilde-Semi> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <tilde-A> <tilde-Semi> <not-Tilde>+ <tilde-A> <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <tilde-OBrace> <tilde-A> <tilde-Caret> <not-Tilde>+ <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <tilde-OBrace> <tilde-A> <tilde-Caret> <not-Tilde>+ <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <tilde-OBrace> <tilde-A> <tilde-Caret> <not-Tilde>+ <tilde-CBrace> <tilde-Semi> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <tilde-OBrace> <tilde-A> <tilde-Caret> <not-Tilde>+ <tilde-CBrace> <tilde-Semi> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <tilde-T> <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <tilde-T> <tilde-Semi> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <tilde-T> <tilde-Semi> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <tilde-T> <tilde-Semi> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <tilde-T> <tilde-Semi> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-T> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-Under> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <not-Tilde>+ <tilde-W> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <tilde-A> <tilde-CAngle>
	| <tilde-OAngle> <tilde-A> <tilde-Caret> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <tilde-A> <tilde-Semi> <tilde-A> <tilde-CAngle>
	| <tilde-OAngle> <tilde-CAngle>
	| <tilde-OAngle> <tilde-CAngle> <tilde-OAngle> <tilde-Semi> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OAngle> <not-Tilde>+ <tilde-Semi> <not-Tilde>+ <tilde-Caret> <tilde-CAngle> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OAngle> <tilde-A> <tilde-Caret> <not-Tilde>+ <tilde-CAngle> <not-Tilde>+ <tilde-CAngle>
	| <tilde-OAngle> <tilde-OBrace> <tilde-A> <tilde-Caret>  <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OBrace> <tilde-A> <tilde-Caret> <not-Tilde>+ <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OBrace> <tilde-A> <tilde-Caret> <not-Tilde>+ <tilde-Under> <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OBrace> <tilde-A> <tilde-Caret> <non-Tilde>+ <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OBrace> <tilde-A> <tilde-Caret> <non-Tilde>+ <tilde-Under> <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OBrace> <tilde-A> <tilde-Caret> <non-Tilde>+ <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OSlash> 'pprint-tabular' <tilde-CSlash> <tilde-CAngle>
	| <tilde-OAngle> <tilde-Semi> <tilde-A> <tilde-CAngle>
	| <tilde-OAngle> <tilde-Semi> <tilde-A> <tilde-Semi> 'bar' <tilde-A> <tilde-CAngle>
	| <tilde-OAngle> <tilde-Semi> <tilde-A> <tilde-Semi> <tilde-CAngle>
	| <tilde-OAngle> <tilde-Semi> <tilde-CAngle> <tilde-OAngle> <tilde-CAngle>
	| <tilde-OAngle> <tilde-Semi> <tilde-OBrace> <tilde-A> <tilde-Caret> '/' <tilde-CBrace> <tilde-Semi> ']' <tilde-CAngle>
	| <tilde-OBrace> 'X ' <tilde-A> ' Y Z' <tilde-CBrace>
	| <tilde-OBrace> 'X ' <tilde-A> <tilde-Caret> ' Y ' <tilde-A> <tilde-Caret> ' ' <tilde-CBrace>
	| <tilde-OBrace> 'X Y Z' <tilde-CBrace>
	| <tilde-OBrace> 'X' <tilde-CBrace>
	| <tilde-OBrace> 'XYZ' <tilde-CBrace>
	| <tilde-OBrace> <not-Tilde>+ <tilde-A> <not-Tilde>+ <tilde-A> ')' <tilde-CBrace>
	| <tilde-OBrace> <not-Tilde>+ <tilde-CBrace>
	| <tilde-OBrace> <not-Tilde>+ <tilde-CBrace>
	| <tilde-OBrace> <not-Tilde>+ <tilde-CBrace>
	| <tilde-OBrace> <not-Tilde>+ <tilde-CBrace>
	| <tilde-OBrace> <tilde-A> ' ' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-A> <tilde-A> <tilde-Caret> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-A> <tilde-Caret> <tilde-A> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-A> <tilde-Caret> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-A> <tilde-Ques> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-CBrace>
	| <tilde-OBrace> <tilde-Caret> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-Caret> <tilde-A> <tilde-CBrace> 'X' <tilde-A>
	| <tilde-OBrace> <tilde-Caret> <tilde-A> <tilde-CBrace> <tilde-A>
	| <tilde-OBrace> <tilde-Caret> <tilde-A> <tilde-Caret> <tilde-A> <tilde-Caret> <tilde-A> <tilde-Caret> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-OBrace> <tilde-A> <tilde-CBrace> <tilde-CBrace>
	| <tilde-OBrace> <tilde-OBracket> 'X' <tilde-Semi> 'Y' <tilde-Caret> 'NO' <tilde-Semi> 'Z' <tilde-Semi> <tilde-Caret> <tilde-CBracket> <tilde-CBrace>
	| <tilde-OBrace> <tilde-OBracket> 'X' <tilde-Semi> 'Y' <tilde-Semi> 'Z' <tilde-Semi> ';' <tilde-Caret> <tilde-CBracket> <tilde-CBrace>
	| <tilde-OBrace> <tilde-OBracket> 'X' <tilde-Semi> 'Y' <tilde-Semi> 'Z' <tilde-Semi> <tilde-Caret> <tilde-CBracket> <tilde-CBrace>
	| <tilde-OBrace> <tilde-OParen> <tilde-C> 'A ' <tilde-C> 'b ' <tilde-Caret> <tilde-C> <tilde-CParen> 'V' <tilde-CBrace>
	| <tilde-OBrace> <tilde-OParen> <tilde-C> 'A ' <tilde-C> 'b ' <tilde-Caret> <tilde-C> <tilde-CParen> 'W' <tilde-CBrace>
	| <tilde-OBrace> <tilde-OParen> <tilde-C> <tilde-C> <tilde-Caret> <tilde-C> <tilde-CParen> 'U' <tilde-CBrace>
	| <tilde-OBrace> <tilde-OParen> <tilde-C> <tilde-C> <tilde-Caret> <tilde-C> <tilde-CParen> 'W' <tilde-CBrace>
	| <tilde-OBracket> 'A' <tilde-Semi> 'B' <tilde-CBracket>
	| <tilde-OBracket> 'X' <tilde-CBracket> 'Y' <tilde-A>
	| <tilde-OBracket> 'a' <tilde-CBracket>
	| <tilde-OBracket> 'a' <tilde-Semi> 'b' <tilde-CBracket>
	| <tilde-OBracket> 'a' <tilde-Semi> 'b' <tilde-Semi> 'c' <tilde-Semi> 'd' <tilde-CBracket>
	| <tilde-OBracket> 'a' <tilde-Semi> 'b' <tilde-Semi> 'c' <tilde-Semi> 'd' <tilde-Semi> 'e' <tilde-CBracket>
	| <tilde-OBracket> 'a' <tilde-Semi> 'b' <tilde-Semi> 'c' <tilde-Semi> 'd' <tilde-Semi> 'e' <tilde-Semi> 'f' <tilde-Semi> 'g' <tilde-Semi> 'h' <tilde-Semi> 'i' <tilde-CBracket>
	| <tilde-OBracket> <tilde-CBracket>
	| <tilde-OBracket> <tilde-Semi> 'a' <tilde-CBracket>
	| <tilde-OParen> '!@#$%^&*this is a TEST.' <tilde-CParen>
	| <tilde-OParen> 'XXyy' <tilde-A> 'uuVV' <tilde-CParen>
	| <tilde-OParen> 'aBc ' <tilde-OParen> 'def' <tilde-CParen> ' GHi' <tilde-CParen>
	| <tilde-OParen> 'this is AlSo A teSt' <tilde-CParen>
	| <tilde-OParen> 'this is a TEST.' <tilde-CParen>
	| <tilde-OParen> 'this is7a TEST.' <tilde-CParen>
	| <tilde-OParen> <tilde-C> <tilde-CParen>
	| <tilde-OSlash> 'CL-TEST::FUNCTION-FOR-FORMAT-SLASH-9' <tilde-CSlash>
	| <tilde-OSlash> 'PPRINT-LINEAR' <tilde-CSlash>
	| <tilde-OSlash> 'cL-tESt:FUNCTION:FOR::FORMAT:SLASH:11' <tilde-CSlash>
	| <tilde-OSlash> 'cl-test::function-for-format-slash-19' <tilde-CSlash>
	| <tilde-OSlash> 'cl-test:FUNCTION-FOR-FORMAT-SLASH-10' <tilde-CSlash>
	| <tilde-OSlash> 'pPrINt-lINeaR' <tilde-CSlash>
	| <tilde-OSlash> 'pprint-linear' <tilde-CSlash>
	| <tilde-P>
	| <tilde-Percent>
	| <tilde-Percent> 'A' <tilde-Under>
	| <tilde-Percent> 'X ' <tilde-OAngle> <tilde-Percent> 'X ' <tilde-Semi> 'AAA' <tilde-CAngle> ',' <tilde-OAngle> <tilde-Percent> 'X ' <tilde-Semi> 'BBB' <tilde-CAngle> ',' <tilde-OAngle> <tilde-Percent> 'X ' <tilde-Semi> 'CCC' <tilde-CAngle>
	| <tilde-Percent> 'X ' <tilde-OAngle> <tilde-Percent> 'X ' <tilde-Semi> 'AAA' <tilde-CAngle> <tilde-OAngle> <tilde-Percent> 'X ' <tilde-Semi> 'BBB' <tilde-CAngle> <tilde-OAngle> <tilde-Percent> 'X  ' <tilde-Semi> 'CCC' <tilde-CAngle>
	| <tilde-Percent> 'X ' <tilde-OAngle> <tilde-Percent> 'X ' <tilde-Semi> 'AAA' <tilde-CAngle> <tilde-OAngle> <tilde-Percent> 'X ' <tilde-Semi> 'BBB' <tilde-CAngle> <tilde-OAngle> <tilde-Percent> 'X ' <tilde-Semi> 'CCC' <tilde-CAngle>
	| <tilde-Percent> 'X ' <tilde-OAngle> <tilde-Percent> 'X ' <tilde-Semi> 'AAA' <tilde-Semi> 'BBB' <tilde-Semi> 'CCC' <tilde-CAngle>
	| <tilde-Pipe>
	| <tilde-Ques>
	| <tilde-Ques> ' ' <tilde-A>
	| <tilde-R>
	| <tilde-S>
	| <tilde-T>
	| <tilde-T> <tilde-T>
	| <tilde-Tilde>
	| <tilde-Tilde> ',,,,\'' <tilde-C> <not-Tilde>+
	| <tilde-Tilde> ',,\'' <tilde-C> ':' <tilde-C>
	| <tilde-Tilde> ',,\'' <tilde-C> <not-Tilde>+
	| <tilde-Tilde> <tilde-D> ',' <tilde-D> <not-Tilde>+
	| <tilde-Tilde> <tilde-D> ',,,\'' <tilde-C> '<' <tilde-Tilde> 'A' <tilde-Tilde> <not-Tilde>+
	| <tilde-Tilde> <tilde-D> ',\'' <tilde-C> <not-Tilde>+
	| <tilde-Tilde> <tilde-D> ',\'' <tilde-C> <tilde-C>
	| <tilde-Tilde> <tilde-D> '@' <tilde-C>
	| <tilde-Tilde> <tilde-D> <not-Tilde>+
	| <tilde-Tilde> <tilde-D> <tilde-C>
	| <tilde-Tilde> <tilde-D> <tilde-Tilde>
	| <tilde-Tilde> <tilde-R>
	| <tilde-Under> 'A' <tilde-Percent>
	| <tilde-Under> <tilde-OAngle> 'X' <tilde-Semi> 'Y' <tilde-CAngle>
	| <tilde-W> <tilde-OAngle> 'X' <tilde-Semi> 'Y' <tilde-CAngle>
	| <tilde-W> <tilde-W> <tilde-Under> <tilde-W> <tilde-W> <tilde-Under> <tilde-W> <tilde-W> <tilde-Under> <tilde-W> <tilde-W> <tilde-Under> <tilde-W> <tilde-W> <tilde-Under>
	| <tilde-X>
	}
}
