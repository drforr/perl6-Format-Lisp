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

	token tilde-Tilde {
	|	'~~'
	}

	token V { <[ v V ]> }

	token tilde-A {
	'~'	[
		|	'#'
		|	'#,#'
		|	'#,#@'
		|	'#@'
		|	'10,,,' <V>
		|	'10,,,v@'
		|	'3,' <unsigned-integer>
		|	'3,,' <V>
		|	'3,,' <signed-integer>
		|	'3,,v@'
		|	'3,3@'
		|	'4,#'
		|	'4,#@'
		|	'4,' <unsigned-integer>
		|	'4,,' <V>
		|	'4,,,'
		|	'4,,,@'
		|	'4,,,\'X'
		|	'4,,,\'X@'
		|	'4,4@'
		|	'5,#'
		|	'5,#@'
		|	'5,' <V>
		|	'5,' <unsigned-integer>
		|	'5,3@'
		|	'5,v@'
		|	'6,' <V>
		|	'7,' <unsigned-integer>
		|	'7,3@'
		|	':'
		|	'@'
		|	'V:'
		|	'V:@'
		|	'V@'
		|	'V@:'
		|	'v,,' <unsigned-integer>
		|	'v:'
		|	'v:@'
		|	'v@'
		|	'v@:'
		|	<V>
		|	<signed-integer>
		]?
	<[ a A ]>
	}

	token tilde-B {
	'~'	[
		|	'#'
		|	'+10@'
		|	',,,#:'
		|	',,,#@:'
		|	',,V,V:'
		|	',,V,V@:'
		|	',,\'*,v:'
		|	',,v,v:'
		|	',,v,v:@'
		|	',,v:'
		|	'6,' <V>
		|	':'
		|	':@'
		|	'@'
		|	'@:'
		|	'V,V,V,' <V>
		|	'v,' <V>
		|	'v,v,v,' <V>
		|	<V>
		|	<signed-integer>
		]?
	<[ b B ]>
	}

	token tilde-C {
	'~'	[
		|	':'
		|	':@'
		|	'@'
		|	'@:'
		]?
	<[ c C ]>
	}

	token tilde-D {
	'~'	[
		|	'#'
		|	'+10@'
		|	',,,#:'
		|	',,,#:@'
		|	',,,#@:'
		|	',,\'*,v:'
		|	',,v,v:'
		|	',,v,v:@'
		|	',,v:'
		|	'6,v'
		|	':'
		|	'@'
		|	'@:'
		|	'v,' <V>
		|	'v,v,v,' <V>
		|	'v,v@'
		|	<V>
		|	<signed-integer>
		]?
	<[ d D ]>
	}

	token tilde-F {
	'~'	[
		| ',' <V>
		| ',' <unsigned-integer>
		| ',,' <V>
		| ',,' <unsigned-integer>
		| ',,,,' <V>
		| ',,,,\','
		| ',,,v'
		| '0,' <unsigned-integer>
		| '1,1,,'
		| '10,1,,'
		| '10,1,,,\'*'
		| '2,' <unsigned-integer>
		| '3,' <unsigned-integer>
		| '3@'
		| '4,' <unsigned-integer>
		| '4,0,,\'*'
		| '4,2,' <signed-integer>
		| '4,2@'
		| '4@'
		| '5,1,,\'*'
		| 'v,' <V>
		| 'v,v,v,v,' <V>
		| <V>
		| <unsigned-integer>
		]?
	<[ f F ]>
	}

	token tilde-O {
	'~'	[
		| '#'
		| '+10@'
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
		| 'd'
		| 'v,' <V>
		| 'v,V@'
		| 'v,v,v,' <V>
		| 'v,v@'
		| <V>
		| <signed-integer>
		| <unsigned-integer> <V>
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
		| '#'
		| '10,#'
		| '10,' <V>
		| '10,' <signed-integer>
		| '10,,,v:'
		| '10,12,' <V>
		| '16,,,,#:'
		| <unsigned-integer>
		| '2,,,,' <unsigned-integer>
		| '2,12,,\'*:'
		| '2:'
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
		| <V>
		| <signed-integer>
		]?
	<[ r R ]>
	}

	token tilde-S {
	'~'	[
		| '10,,,' <V>
		| '10,,,v@'
		| '3,' <unsigned-integer>
		| '3,,' <V>
		| '3,,' <signed-integer>
		| '3,,V@'
		| '3,3@'
		| '4,' <unsigned-integer>
		| '4,,' <V>
		| '4,,,'
		| '4,,,@'
		| '4,,,\'X'
		| '4,,,\'X@'
		| '4,4@'
		| '5,' <V>
		| '5,' <signed-integer>
		| '5,3@'
		| '5,v@'
		| '7,' <unsigned-integer>
		| '7,3@'
		| ':'
		| '@'
		| 'V,,' <unsigned-integer>
		| 'V:'
		| 'V@:'
		| 'v,,' <unsigned-integer>
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
		| '0,' <V>
		| '0,' <unsigned-integer>
		| '0,v@'
		| '1,' <unsigned-integer>
		| '1,1@'
		| '2,' <unsigned-integer>
		| 'v,' <V>
		| 'v,' <unsigned-integer>
		| 'v,1@'
		| 'v,v@'
		| '10,20:@'
		| '10,20@:'
		| '10:'
		]?
	<[ t T ]>
	}

	token tilde-X {
	'~'	[
		| '#'
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
		| <V>
		| <signed-integer>
		]?
	<[ x X ]>
	}

	token tilde-Caret {
	'~'	[
		|	<unsigned-integer> ',#'
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
		|	<V>
		|	<unsigned-integer>
		|	<unsigned-integer> ':'
		|	<unsigned-integer> '@'
		]?
	'*'
	}

	token tilde-OBrace {
	'~'	[
		|	'@'
		|	<unsigned-integer>
		]?
	'{'
	}

	token tilde-CBrace {
	# {
	|	'~}'
	}

	token tilde-OBracket {
	|	'~@['
	}

	token tilde-CBracket {
	|	'~]'
	}

	token TOP {
	| ' ' <tilde-T>
	| '#\\\\' <tilde-C>
	| 'X'
	| 'XXXXX' <tilde-T>
	| '\'' <tilde-C>
	| <tilde-A>
	| <tilde-A> 'Y' <tilde-Ques> 'X' <tilde-A>
	| <tilde-A> <tilde-A> <tilde-Star> <tilde-A>
	| <tilde-A> <tilde-A> <tilde-Star> <tilde-A> <tilde-A>
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
	| <tilde-A> <tilde-Tilde> <tilde-D> ',' <tilde-D> 'T'
	| <tilde-B>
	| <tilde-C>
	| <tilde-D>
	| <tilde-D> ' cat' <tilde-P>
	| <tilde-D> ' penn' <tilde-P>
	| <tilde-D> 'b'
	| <tilde-D> 'd'
	| <tilde-D> 'o'
	| <tilde-D> 'r'
	| <tilde-D> 'x'
	| <tilde-F>
	| <tilde-O>
	| <tilde-OBrace> <tilde-Caret> <tilde-A> <tilde-CBrace> 'X' <tilde-A>
	| <tilde-OBrace> <tilde-Caret> <tilde-A> <tilde-CBrace> <tilde-A>
	| <tilde-OBracket> 'X' <tilde-CBracket> 'Y' <tilde-A>
	| <tilde-P>
	| <tilde-Ques> ' ' <tilde-A>
	| <tilde-R>
	| <tilde-S>
	| <tilde-T>
	| <tilde-T> <tilde-T>
	| <tilde-Tilde> ',,,,\'' <tilde-C> 'f'
	| <tilde-Tilde> ',,\'' <tilde-C> ':' <tilde-C>
	| <tilde-Tilde> ',,\'' <tilde-C> ':d'
	| <tilde-Tilde> <tilde-D> ',' <tilde-D> ',\'*r'
	| <tilde-Tilde> <tilde-D> ',' <tilde-D> '@t'
	| <tilde-Tilde> <tilde-D> ',' <tilde-D> 'R'
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
	| <tilde-Tilde> <tilde-D> <tilde-C>
	| <tilde-Tilde> <tilde-R>
	| <tilde-X>
	| 'XX' <tilde-T> 'YY'
	| 'X' <tilde-A> 'Y'
	}
}
