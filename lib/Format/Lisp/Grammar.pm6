=begin pod

=begin NAME

Format::Lisp::Grammar - Grammar for Common Lisp format strings

=end NAME

=begin DESCRIPTION

=end DESCRIPTION

=end pod

grammar Format::Lisp::Grammar {
	token signed-integer {
	|	<[ - + ]>? [
		|	<[ 1..9 ]> <[ 0..9 ]>*
		|	0
	]
	}

	token tilde-Tilde {
	|	'~~'
	}

	token tilde-A {
	'~'	[
		|	'#,#@'
		|	'#@'
		|	'#,#'
		|	'#'
		|	'V@:'
		|	'V@'
		|	'@'
		|	':'
		|	'V:@'
		|	'V:'
		|	'V'
		|	'v@:'
		|	'v@'
		|	'v:@'
		|	'v:'
		|	'v,,2'
		|	'v'
		]?
	<[ a A ]>
	}

	token tilde-B {
	'~'	[
		|	'V,V,V,V'
		|	',,V,V@:'
		|	',,V,V:'
		|	':@'
		|	'#'
		|	'@:'
		|	'@'
		|	':'
		|	'v,v,v,v'
		|	'v,v'
		|	'v'
		|	',,,#@:'
		|	',,,#:'
		|	',,\'*,v:'
		|	',,v,v:@'
		|	',,v,v:'
		|	',,v:'
		]?
	<[ b B ]>
	}

	token tilde-C {
	'~'	[
		|	':@'
		|	'@:'
		|	'@'
		|	':'
		]?
	<[ c C ]>
	}

	token tilde-D {
	'~'	[
		|	'#'
		|	'@:'
		|	'@'
		|	':'
		|	'v,v,v,v'
		|	'v,v@'
		|	'v,v'
		|	'v'
		|	',,,#:@'
		|	',,,#@:'
		|	',,,#:'
		|	',,\'*,v:'
		|	',,v,v:@'
		|	',,v,v:'
		|	',,v:'
		]?
	<[ d D ]>
	}

	token tilde-Caret {
	'~'	[
		|	'2,#'
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
		|	'4:'
		|	'3:'
		|	'2:'
		|	'0:'
		|	':'
		|	'3@'
		|	'2@'
		|	'1@'
		|	'0@'
		|	'v@'
		|	'@'
		|	'v:'
		|	'v'
		]?
	'*'
	}

	token tilde-OBrace {
	'~'	[
		|	'@'
		|	'1'
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
	| '~+10@B'
	| '~+10@d'
	| '~+10b'
	| '~+10d'
	| '~-100000000000000000000a'
	| '~-1000000000000000000B'
	| '~-1000000000000000000d'
	| '~-100A'
	| '~-1b'
	| '~-1d'
	| '~10,,,v@A'
	| '~10,,,v@a'
	| '~10,,,vA'
	| '~10,,,va'
	| '~3,,+2A'
	| '~3,,-1A'
	| '~3,,0A'
	| '~3,,v@A'
	| '~3,,vA'
	| '~3,1a'
	| '~3,3@a'
	| '~4,#@A'
	| '~4,#A'
	| '~4,,,@A'
	| '~4,,,\'X@a'
	| '~4,,,\'XA'
	| '~4,,,a'
	| '~4,,va'
	| '~4,3a'
	| '~4,4@a'
	| '~5,#@A'
	| '~5,#a'
	| '~5,3@a'
	| '~5,3A'
	| '~5,v@A'
	| '~5,vA'
	| '~6,vB'
	| '~6,vD'
	| '~7,3@a'
	| '~7,3A'
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
