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

	token value {
	|	<signed-integer>
	|	<V>
	|	'#'
	}

	token tilde-Tilde {
	'~'	[
		|	'#'
		|	<V>
		]?
	'~'
	}

	token tilde-A {
	'~'	[
		| '#@'
		| '10,,,' <V>
		| '10,,,v@'
		| <value> ',' <value> '@'
		| <value> ',' <value>
		| '3,,' <value>
		| '3,,v@'
		| '4,,' <V>
		| '4,,,'
		| '4,,,@'
		| '4,,,\'X'
		| '4,,,\'X@'
		| ':'
		| '@'
		| 'V:'
		| 'V:@'
		| 'V@'
		| 'V@:'
		| 'v,,' <signed-integer>
		| 'v:'
		| 'v:@'
		| 'v@'
		| 'v@:'
		| <value>
		]?
	<[ a A ]>
	}

	token tilde-B {
	'~'	[
		| ',,,#:'
		| ',,,#@:'
		| ',,V,V:'
		| ',,V,V@:'
		| ',,\'*,v:'
		| ',,v,v:'
		| ',,v,v:@'
		| ',,v:'
		| '6,' <V>
		| ':'
		| ':@'
		| '@'
		| '@:'
		| 'V,V,V,' <V>
		| 'v,' <V>
		| 'v,v,v,' <V>
		| <value> '@'
		| <value>
		]?
	<[ b B ]>
	}

	token tilde-C {
	'~'	[
		| ':'
		| ':@'
		| '@'
		| '@:'
		]?
	<[ c C ]>
	}

	token tilde-D {
	'~'	[
		| '+10@'
		| ',,,#:'
		| ',,,#:@'
		| ',,,#@:'
		| ',,\'*,v:'
		| ',,v,v:'
		| ',,v,v:@'
		| ',,v:'
		| '6,' <V>
		| ':'
		| '@'
		| '@:'
		| 'v,' <V>
		| 'v,v,v,' <V>
		| 'v,v@'
		| <value>
		]?
	<[ d D ]>
	}

	token tilde-F {
	'~'	[
		| ',' <V>
		| ',' <signed-integer>
		| ',,' <V>
		| ',,' <signed-integer>
		| ',,,,' <V>
		| ',,,,\','
		| ',,,v'
		| '0,' <signed-integer>
		| '1,1,,'
		| '10,1,,'
		| '10,1,,,\'*'
		| '2,' <signed-integer>
		| '3,' <signed-integer>
		| '4,' <signed-integer>
		| '4,0,,\'*'
		| '4,2,' <signed-integer>
		| '4,2@'
		| '5,1,,\'*'
		| 'v,' <V>
		| 'v,v,v,v,' <V>
		| <value> '@'
		| <value>
		]?
	<[ f F ]>
	}

	token tilde-I {
	'~'	[
		| ':'
		| 'v:'
		| <value> ':'
		| <value>
		]?
	<[ i I ]>
	}

	token tilde-O {
	'~'	[
		| ',,,#:'
		| ',,,#:@'
		| ',,,#@:'
		| ',,V,v:'
		| ',,\'*,v:'
		| ',,v,V@:'
		| ',,v,v:'
		| ',,v,v:@'
		| ',,v:'
		| '6,' <V>
		| ':'
		| ':@'
		| '@'
		| '@:'
		| 'V,' <V>
		| 'v,' <V>
		| 'v,V@'
		| 'v,v,v,' <V>
		| 'v,v@'
		| <value> '@'
		| <value>
		| <signed-integer> <V>
		]?
	<[ o O ]>
	}

	token tilde-P {
	'~'	[
		| ':@'
		| ':'
		| '@:'
		| '@'
		]?
	<[ p P ]>
	}

	token tilde-R {
	'~'	[
		| '10,#'
		| '10,' <V>
		| '10,' <signed-integer>
		| '10,,,v:'
		| '10,12,' <V>
		| '16,,,,#:'
		| '2,,,,' <signed-integer>
		| '2,12,,\'*:'
		| '3,14,\'X,\',:'
		| '3@:'
		| '8,,,,v:'
		| '8,10:@'
		| '8@'
		| ':'
		| ':@'
		| '@'
		| '@:'
		| 'v,v,v,v,' <V>
		| <value> ':'
		| <value>
		]?
	<[ r R ]>
	}

	token tilde-S {
	'~'	[
		| '10,,,' <V>
		| '10,,,v@'
		| '3,,' <V>
		| '3,,' <signed-integer>
		| '3,,V@'
		| '3,3@'
		| '4,,' <V>
		| '4,,,'
		| '4,,,@'
		| '4,,,\'X'
		| '4,,,\'X@'
		| '4,4@'
		| <value> ',' <value>
		| '5,3@'
		| '5,v@'
		| '7,3@'
		| ':'
		| '@'
		| 'V,,' <signed-integer>
		| 'V:'
		| 'V@:'
		| 'v,,' <signed-integer>
		| 'v:'
		| 'v:@'
		| 'v@'
		| 'v@:'
		| <V>
		]?
	<[ s S ]>
	}

	token tilde-T {
	'~'	[
		| '0,' <V> ':'
		| <value> ',' <value>
		| '0,' <signed-integer> ':'
		| '0,' ':' 
		| '0,v@'
		| '1,' <signed-integer> ':'
		| '1,1:@'
		| '1,1@'
		| '1,:@'
		| '2,' <signed-integer> ':'
		| 'v,' <signed-integer> ':@'
		| 'v,' <signed-integer> ':'
		| 'v,1@'
		| 'v,v:@'
		| 'v,v:'
		| 'v,v@'
		| '10,20:@'
		| '10,20@:'
		| '10:'
		| '1:@'
		| ':@'
		| ',1:@'
		| ',0:'
		| '0:'
		]?
	<[ t T ]>
	}

	token tilde-Z {
	'~'	[
		| '?'
		| '@?'
		]?
	<[ z Z ]>
	}

	token tilde-W {
	'~'	#[
		#]?
	<[ w W ]>
	}

	token tilde-X {
	'~'	[
		| '+10@'
		| ',,,#:'
		| ',,,#@:'
		| ',,V:'
		| ',,\'*,v:'
		| ',,v,V:@'
		| ',,v,v:'
		| ',,v,v:@'
		| ',,v:'
		| '6,' <V>
		| ':'
		| ':@'
		| '@'
		| '@:'
		| 'V,' <V>
		| 'v,' <V>
		| 'v,V@'
		| 'v,v,v,' <V>
		| 'v,v@'
		| <value>
		]?
	<[ x X ]>
	}

	token tilde-Caret {
	'~'	[
		| <signed-integer> ',#:'
		| <signed-integer> ',#'
		| <V> ',' <V> ',' <V> ':'
		| <V> ',' <V> ',' <V>
		| <V> ',' <V> ':'
		| <V> ',' <V>
		| <V> ',' <signed-integer> ':'
		| <V> ',' <signed-integer>
		| <V> ':'
		| <signed-integer> ':'
		| ':'
		| <value>
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
		|	'v:'
		|	'v@'
		|	<value>
		|	<signed-integer> ':'
		|	<signed-integer> '@'
		]?
	'*'
	}

	token tilde-OParen {
	'~'	[
		| ':@'
		| '@:'
		| '@'
		| ':'
		| <signed-integer>
		]?
	'('
	}

	token tilde-CParen {
	'~'	[
		| ':@'
		| '@'
		| ':'
		| <signed-integer>
		]?
	')'
	}

	token tilde-OBrace {
	'~'	[
		|	<V> ':@'
		|	':@'
		|	<V> '@:'
		|	'@:'
		|	'@'
		|	<V> ':'
		|	<V> '@'
		|	':'
		|	'#:@'
		|	'#@'
		|	'#:'
		|	<signed-integer> ':@'
		|	<signed-integer> ':'
		|	<signed-integer> '@'
		|	<value>
		]?
	'{'
	}

	token tilde-CBrace {
	# {
	'~'	[
		| ':'
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
		| '0,30:'
		| '0,3:'
		| '@'
		| ':'
		]?
	';'
	}

	token tilde-CBracket {
	|	'~]'
	}

	token tilde-OAngle {
	'~'	[
		|	',,1,\','
		|	<V> ',,,' <V>
		|	',,' <signed-integer> ',' <V>
		|	',,' <signed-integer> ','
		|	<signed-integer> ',,' <signed-integer>
		|	',,' <signed-integer>
		|	<V> ',,' <V>
		|	',,' <V>
		|	',' <V>
		|	'@:'
		|	'6@'
		|	'@'
		|	<signed-integer> ':@'
		|	':@'
		|	':'
		|	<signed-integer> ':'
		|	<signed-integer> '@'
		|	<value>
		]?
	'<'
	}

	token tilde-CAngle {
	'~'	[
		|	'@'
		|	':@'
		|	':'
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

	token tilde-Under {
	'~'	[
		|	':@'
		|	':'
		|	'@:'
		|	'@'
		]?
	'_'
	}

	token tilde-OSlash {
	'~'	[
		| '\',@'
		| '\'X:'
		| '-1@'
		| '1,2,3,4,5,6,7,8,9,10@'
		| 'v,v,v,v,v,v,v,v,v,v@'
		| '18@:'
		| ':@'
		| '4:'
		| ':'
		| 'v:'
		| '@:'
		| '@'
		| <V>
		]?
	'/'
	}

	token tilde-CSlash {
		'/'
	}

	token TOP {
	| ' ' <tilde-T>
	| '#\\\\' <tilde-C>
	| '(' <tilde-A> ' ' <tilde-A> ')'
	| '(' <tilde-OBrace> <tilde-A> <tilde-Caret> ',' <tilde-CBrace> ')'
	| 'A             ' <tilde-Under>
	| 'A ' <tilde-Under> 'A '
	| 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under>
	| 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under>
	| 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under>
	| 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> <tilde-Percent> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under> 'A ' <tilde-Under>
	| 'AAAA ' <tilde-Under>
	| 'B ' <tilde-Under>
	| 'D ' <tilde-Under>
	| 'X'
	| 'X' <tilde-A> 'Y'
	| 'X' <tilde-Amp>
	| 'X' <tilde-Percent>
	| 'X' <tilde-Percent> <tilde-Amp>
	| 'X' <tilde-Tilde> <tilde-D> '&'
	| 'XX' <tilde-T> 'YY'
	| 'XXX' <tilde-OAngle> 'MMM' <tilde-I> <tilde-Under> 'MMMMM' <tilde-CAngle>
	| 'XXX' <tilde-OAngle> 'MMMI' <tilde-I> <tilde-Under> 'MMMMM' <tilde-CAngle>
	| 'XXXXX' <tilde-T>
	| '\'' <tilde-C>
	| '\'' <tilde-C> ','
	| 'a' <tilde-Z>
	| <tilde-A>
	| <tilde-A> 'Y' <tilde-Ques> 'X' <tilde-A>
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
	| <tilde-A> <tilde-Ques> 'X' <tilde-A>
	| <tilde-A> <tilde-Star> <tilde-A>
	| <tilde-A> <tilde-T>
	| <tilde-A> <tilde-Tilde> '<' <tilde-A> <tilde-Tilde> <tilde-D> ',' <tilde-D> ':T' <tilde-Tilde> ':>'
	| <tilde-A> <tilde-Tilde> <tilde-D> ',' <tilde-D> 'T'
	| <tilde-Amp>
	| <tilde-B>
	| <tilde-C>
	| <tilde-D>
	| <tilde-D> ' cat' <tilde-P>
	| <tilde-D> ' penn' <tilde-P>
	| <tilde-D> ','
	| <tilde-D> 'b'
	| <tilde-D> 'd'
	| <tilde-D> 'o'
	| <tilde-D> 'r'
	| <tilde-D> 'x'
	| <tilde-F>
	| <tilde-O>
	| <tilde-OAngle> '(' <tilde-Semi> 'M' <tilde-I> <tilde-Under> 'M' <tilde-Semi> ')' <tilde-CAngle>
	| <tilde-OAngle> '**' <tilde-Semi> <tilde-OBrace> <tilde-A> <tilde-Caret> '       ' <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> '**' <tilde-Semi> <tilde-OBrace> <tilde-A> <tilde-Caret> '       ' <tilde-CBrace> <tilde-Semi> 'XX' <tilde-CAngle>
	| <tilde-OAngle> 'ABC' <tilde-Semi> <tilde-T> <tilde-Semi> 'DEF' <tilde-CAngle>
	| <tilde-OAngle> 'M' <tilde-I> <tilde-Under> 'M' <tilde-CAngle>
	| <tilde-OAngle> 'MMM' <tilde-I> <tilde-Under> 'MMMMM' <tilde-CAngle>
	| <tilde-OAngle> 'X' <tilde-Semi> <tilde-T> <tilde-Semi> 'Y' <tilde-CAngle>
	| <tilde-OAngle> 'XXX' <tilde-Semi> <tilde-T> <tilde-Semi> 'YYY' <tilde-CAngle>
	| <tilde-OAngle> 'XXXX' <tilde-Semi> <tilde-T> <tilde-CAngle>
	| <tilde-OAngle> 'XXXXXX' <tilde-Caret> <tilde-CAngle>
	| <tilde-OAngle> 'XXXXXX' <tilde-Semi> 'YYYYYYY' <tilde-Caret> <tilde-CAngle>
	| <tilde-OAngle> 'XXXXXX' <tilde-Semi> 'YYYYYYY' <tilde-Caret> <tilde-Semi> 'ZZZZZ' <tilde-CAngle>
	| <tilde-OAngle> '[' <tilde-Semi> 'XXXX' <tilde-T> <tilde-Semi> ']' <tilde-CAngle>
	| <tilde-OAngle> '[' <tilde-Semi> <tilde-OBrace> <tilde-A> <tilde-Caret> '/' <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> '[' <tilde-Semi> <tilde-OBrace> <tilde-A> <tilde-Caret> '/' <tilde-CBrace> <tilde-Semi> ']' <tilde-CAngle>
	| <tilde-OAngle> '[' <tilde-Semi> <tilde-T> <tilde-Semi> ']' <tilde-CAngle>
	| <tilde-OAngle> 'aaa' <tilde-Semi> 'bbb' <tilde-Semi> 'ccc' <tilde-CAngle>
	| <tilde-OAngle> 'abc' <tilde-Semi> 'def' <tilde-Caret> <tilde-CAngle>
	| <tilde-OAngle> 'abcdef' <tilde-CAngle>
	| <tilde-OAngle> <tilde-A> <tilde-CAngle>
	| <tilde-OAngle> <tilde-A> <tilde-Caret> 'xxxx' <tilde-CAngle>
	| <tilde-OAngle> <tilde-A> <tilde-Semi> <tilde-A> <tilde-CAngle>
	| <tilde-OAngle> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OAngle> 'XXXXXX' <tilde-Semi> 'YYYYYYY' <tilde-Caret> <tilde-CAngle> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OAngle> <tilde-A> <tilde-Caret> 'xxx' <tilde-CAngle> 'yyy' <tilde-CAngle>
	| <tilde-OAngle> <tilde-OBrace> <tilde-A> <tilde-Caret>  <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OBrace> <tilde-A> <tilde-Caret> '            ' <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OBrace> <tilde-A> <tilde-Caret> '            ' <tilde-Under> <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OBrace> <tilde-A> <tilde-Caret> ' ' <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OBrace> <tilde-A> <tilde-Caret> ' ' <tilde-Under> <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OBrace> <tilde-A> <tilde-Caret> '*' <tilde-CBrace> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OSlash> 'pprint-tabular' <tilde-CSlash> <tilde-CAngle>
	| <tilde-OAngle> <tilde-OSlash> 'pprint-tabular' <tilde-CSlash> <tilde-CAngle>
	| <tilde-OAngle> <tilde-Semi> <tilde-A> <tilde-CAngle>
	| <tilde-OAngle> <tilde-Semi> <tilde-A> <tilde-Semi> <tilde-CAngle>
	| <tilde-OAngle> <tilde-Semi> <tilde-OBrace> <tilde-A> <tilde-Caret> '/' <tilde-CBrace> <tilde-Semi> ']' <tilde-CAngle>
	| <tilde-OBrace> ' ' <tilde-CBrace>
	| <tilde-OBrace> '(' <tilde-A> ' ' <tilde-A> ')' <tilde-CBrace>
	| <tilde-OBrace> 'A' <tilde-CBrace>
	| <tilde-OBrace> 'ABC' <tilde-CBrace>
	| <tilde-OBrace> 'FOO' <tilde-CBrace>
	| <tilde-OBrace> 'X ' <tilde-A> ' Y Z' <tilde-CBrace>
	| <tilde-OBrace> 'X ' <tilde-A> <tilde-Caret> ' Y ' <tilde-A> <tilde-Caret> ' ' <tilde-CBrace>
	| <tilde-OBrace> 'X Y Z' <tilde-CBrace>
	| <tilde-OBrace> 'X' <tilde-CBrace>
	| <tilde-OBrace> 'XYZ' <tilde-CBrace>
	| <tilde-OBrace> '~#,#,#:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,#,#^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,#,2:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,#,3^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,#,v^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,#:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,#^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,1,2^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,1:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,1^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,2,#:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,2,2:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,3,#^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,3,3^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,3^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#,v:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~#^~A~#^~A~#^~A~#^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~(~C~C~0^~C~)W' <tilde-CBrace>
	| <tilde-OBrace> '~0,1:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~0,3,#^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~0,v,v^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~0,v^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,0,1^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,1,1^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,1,v:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,1,v^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,1:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,2,1:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,2,1^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,2,3:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,2,3^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,2,v^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,3,#:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,V:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,v,2:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,v,3^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,v,v^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~1,v^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~2,#,3:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~2,#,3^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~2,1,3:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~2,V,v:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~2,v^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~3,#,#:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~3,#,#^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~3,2,1^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~3,\'x^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~3,v^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~:(' <tilde-C> <tilde-C> '~0^~C~)U' <tilde-CBrace>
	| <tilde-OBrace> '~@(' '~CA ~Cb ~0^~C' '~)' 'V' <tilde-CBrace>
	| <tilde-OBrace> '~@:(' '~CA ~Cb ~0^~C' '~)' 'W' <tilde-CBrace>
	| <tilde-OBrace> '~V,#:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~V,v,3:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~\',,\',^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~\'X,\'X:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~\'X,\'Y:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~\'X,v^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~\'X:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~\'X^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~\'x,3^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~\'x,\'x^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~v,1,v^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~v,2,2:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~v,2,3^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~v,2,v:^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> '~v,\'X^' <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-A> <tilde-A> <tilde-Caret> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-A> <tilde-Caret> <tilde-A> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-A> <tilde-Caret> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-A> <tilde-Ques> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-CBrace>
	| <tilde-OBrace> <tilde-Caret> <tilde-A> <tilde-CBrace>
	| <tilde-OBrace> <tilde-Caret> <tilde-A> <tilde-CBrace> 'X' <tilde-A>
	| <tilde-OBrace> <tilde-Caret> <tilde-A> <tilde-CBrace> <tilde-A>
	| <tilde-OBrace> <tilde-OBrace> <tilde-A> <tilde-CBrace> <tilde-CBrace>
	| <tilde-OBrace> <tilde-OBracket> 'X' <tilde-Semi> 'Y' <tilde-Caret> 'NO' <tilde-Semi> 'Z' <tilde-Semi> <tilde-Caret> <tilde-CBracket> <tilde-CBrace>
	| <tilde-OBrace> <tilde-OBracket> 'X' <tilde-Semi> 'Y' <tilde-Semi> 'Z' <tilde-Semi> ';' <tilde-Caret> <tilde-CBracket> <tilde-CBrace>
	| <tilde-OBrace> <tilde-OBracket> 'X' <tilde-Semi> 'Y' <tilde-Semi> 'Z' <tilde-Semi> <tilde-Caret> <tilde-CBracket> <tilde-CBrace>
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
	| <tilde-Tilde> ',,,,\'' <tilde-C> 'f'
	| <tilde-Tilde> ',,\'' <tilde-C> ':' <tilde-C>
	| <tilde-Tilde> ',,\'' <tilde-C> ':d'
	| <tilde-Tilde> <tilde-D> '%'
	| <tilde-Tilde> <tilde-D> '&'
	| <tilde-Tilde> <tilde-D> ','
	| <tilde-Tilde> <tilde-D> ',' <tilde-D> ',\'*r'
	| <tilde-Tilde> <tilde-D> ',' <tilde-D> '@t'
	| <tilde-Tilde> <tilde-D> ',' <tilde-D> 'R'
	| <tilde-Tilde> <tilde-D> ',,,\'' <tilde-C> '<' <tilde-Tilde> 'A' <tilde-Tilde> '>'
	| <tilde-Tilde> <tilde-D> ',\'' <tilde-C> 'd'
	| <tilde-Tilde> <tilde-D> ',\'' <tilde-C> <tilde-C>
	| <tilde-Tilde> <tilde-D> ':a'
	| <tilde-Tilde> <tilde-D> ':s'
	| <tilde-Tilde> <tilde-D> '@' <tilde-C>
	| <tilde-Tilde> <tilde-D> '@:A'
	| <tilde-Tilde> <tilde-D> '@:S'
	| <tilde-Tilde> <tilde-D> '@a'
	| <tilde-Tilde> <tilde-D> '@b'
	| <tilde-Tilde> <tilde-D> '@d'
	| <tilde-Tilde> <tilde-D> '@o'
	| <tilde-Tilde> <tilde-D> '@s'
	| <tilde-Tilde> <tilde-D> 'S'
	| <tilde-Tilde> <tilde-D> 'a'
	| <tilde-Tilde> <tilde-D> 'b'
	| <tilde-Tilde> <tilde-D> 'd'
	| <tilde-Tilde> <tilde-D> 'o'
	| <tilde-Tilde> <tilde-D> '|'
	| <tilde-Tilde> <tilde-D> <tilde-C>
	| <tilde-Tilde> <tilde-D> <tilde-Tilde>
	| <tilde-Tilde> <tilde-R>
	| <tilde-Under> 'A' <tilde-Percent>
	| <tilde-W> <tilde-W> <tilde-Under> <tilde-W> <tilde-W> <tilde-Under> <tilde-W> <tilde-W> <tilde-Under> <tilde-W> <tilde-W> <tilde-Under> <tilde-W> <tilde-W> <tilde-Under>
	| <tilde-X>
	}
}
