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

	token tilde-A {
	'~'	[
		|	'#'
		|	'#,#'
		|	'#,#@'
		|	'#@'
		|	'-100'
		|	'-100000000000000000000'
		|	'10,,,v'
		|	'10,,,v@'
		|	'3,,+2'
		|	'3,,-1'
		|	'3,,0'
		|	'3,,v'
		|	'3,,v@'
		|	'3,1'
		|	'3,3@'
		|	'4,#'
		|	'4,#@'
		|	'4,,,'
		|	'4,,,@'
		|	'4,,,\'X'
		|	'4,,,\'X@'
		|	'4,,v'
		|	'4,3'
		|	'4,4@'
		|	'5,#'
		|	'5,#@'
		|	'5,3'
		|	'5,3@'
		|	'5,v'
		|	'5,v@'
		|	'6,v'
		|	'7,3'
		|	'7,3@'
		|	':'
		|	'@'
		|	'V'
		|	'V:'
		|	'V:@'
		|	'V@'
		|	'V@:'
		|	'v'
		|	'v,,2'
		|	'v:'
		|	'v:@'
		|	'v@'
		|	'v@:'
		]?
	<[ a A ]>
	}

	token tilde-B {
	'~'	[
		|	'#'
		|	'+10'
		|	'+10@'
		|	',,,#:'
		|	',,,#@:'
		|	',,V,V:'
		|	',,V,V@:'
		|	',,\'*,v:'
		|	',,v,v:'
		|	',,v,v:@'
		|	',,v:'
		|	'-1'
		|	'-1000000000000000000'
		|	'6,v'
		|	':'
		|	':@'
		|	'@'
		|	'@:'
		|	'V,V,V,V'
		|	'v'
		|	'v,v'
		|	'v,v,v,v'
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
		|	'+10'
		|	'+10@'
		|	',,,#:'
		|	',,,#:@'
		|	',,,#@:'
		|	',,\'*,v:'
		|	',,v,v:'
		|	',,v,v:@'
		|	',,v:'
		|	'-1'
		|	'-1000000000000000000'
		|	'6,v'
		|	':'
		|	'@'
		|	'@:'
		|	'v'
		|	'v,v'
		|	'v,v,v,v'
		|	'v,v@'
		]?
	<[ d D ]>
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
		|	'0'
		|	':'
		|	'@'
		|	'v'
		|	'v:'
		|	'v@'
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
	| '#\\\\' <tilde-C>
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
	| <tilde-B>
	| <tilde-C>
	| <tilde-D>
	| <tilde-D> 'b'
	| <tilde-D> 'd'
	| <tilde-OBrace> <tilde-Caret> <tilde-A> <tilde-CBrace> 'X' <tilde-A>
	| <tilde-OBrace> <tilde-Caret> <tilde-A> <tilde-CBrace> <tilde-A>
	| <tilde-OBracket> 'X' <tilde-CBracket> 'Y' <tilde-A>
	| <tilde-Ques> ' ' <tilde-A>
	| <tilde-Tilde> ',,\'' <tilde-C> ':' <tilde-C>
	| <tilde-Tilde> ',,\'' <tilde-C> ':d'
	| <tilde-Tilde> <tilde-D> ',\'' <tilde-C> 'd'
	| <tilde-Tilde> <tilde-D> ',\'' <tilde-C> <tilde-C>
	| <tilde-Tilde> <tilde-D> ':a'
	| <tilde-Tilde> <tilde-D> '@' <tilde-C>
	| <tilde-Tilde> <tilde-D> '@:A'
	| <tilde-Tilde> <tilde-D> '@a'
	| <tilde-Tilde> <tilde-D> '@b'
	| <tilde-Tilde> <tilde-D> '@d'
	| <tilde-Tilde> <tilde-D> 'a'
	| <tilde-Tilde> <tilde-D> 'b'
	| <tilde-Tilde> <tilde-D> 'd'
	| <tilde-Tilde> <tilde-D> <tilde-C>
	}
}
