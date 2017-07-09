=begin pod

=begin NAME

Format::Lisp::Actions - Actions for Common Lisp format strings

=end NAME

=begin DESCRIPTION

=end DESCRIPTION

=end pod

role Stringify {
	has $.text;
	method from-string( Str $x ) {
		return self.bless( text => $x );
	}
}

class Format::Lisp::Text {
	also does Stringify;
}

class Format::Lisp::Directive {
	has @.options;
}

class Format::Lisp::Directive::A is Format::Lisp::Directive { }
class Format::Lisp::Directive::Amp is Format::Lisp::Directive { }
class Format::Lisp::Directive::B is Format::Lisp::Directive { }
class Format::Lisp::Directive::Caret is Format::Lisp::Directive { }
class Format::Lisp::Directive::C is Format::Lisp::Directive { }
class Format::Lisp::Directive::D is Format::Lisp::Directive { }
class Format::Lisp::Directive::E is Format::Lisp::Directive { }
class Format::Lisp::Directive::F is Format::Lisp::Directive { }
class Format::Lisp::Directive::G is Format::Lisp::Directive { }
class Format::Lisp::Directive::I is Format::Lisp::Directive { }
class Format::Lisp::Directive::O is Format::Lisp::Directive { }
class Format::Lisp::Directive::Percent is Format::Lisp::Directive { }
class Format::Lisp::Directive::Pipe is Format::Lisp::Directive { }
class Format::Lisp::Directive::P is Format::Lisp::Directive { }
class Format::Lisp::Directive::Ques is Format::Lisp::Directive { }
class Format::Lisp::Directive::R is Format::Lisp::Directive { }
class Format::Lisp::Directive::Semi is Format::Lisp::Directive { }
class Format::Lisp::Directive::Slash is Format::Lisp::Directive {
	also does Stringify;
}
class Format::Lisp::Directive::Star is Format::Lisp::Directive { }
class Format::Lisp::Directive::S is Format::Lisp::Directive { }
class Format::Lisp::Directive::Tilde is Format::Lisp::Directive { }
class Format::Lisp::Directive::T is Format::Lisp::Directive { }
class Format::Lisp::Directive::Under is Format::Lisp::Directive { }
class Format::Lisp::Directive::W is Format::Lisp::Directive { }
class Format::Lisp::Directive::X is Format::Lisp::Directive { }

class Format::Lisp::Actions {
	method signed-integer( $/ ) {
	}

	method unsigned-integer( $/ ) {
	}

	method V( $/ ) {
	}

	method not-Tilde( $/ ) {
		make Format::Lisp::Text.from-string( ~$/ )
	}

	method value( $/ ) {
	}

	method options( $/ ) {
	}

	method value-comma( $/ ) {
		make ~$/<value>
	}

	method Tilde-Options( $/ ) {
	}

	method tilde-OAngle( $/ ) {
	}

	method tilde-CAngle( $/ ) {
	}

	method tilde-OBrace( $/ ) {
	}

	method tilde-CBrace( $/ ) {
	}

	method tilde-OBracket( $/ ) {
	}

	method tilde-CBracket( $/ ) {
	}

	method tilde-OParen( $/ ) {
	}

	method tilde-CParen( $/ ) {
	}

	method Atom( $/ ) {
		if $/<not-Tilde> { make $/<not-Tilde>.ast }
		elsif $/<tilde-A> {
			make Format::Lisp::Directive::A.new
		}
		elsif $/<tilde-Amp> {
			make Format::Lisp::Directive::Amp.new
		}
		elsif $/<tilde-B> {
			make Format::Lisp::Directive::B.new
		}
		elsif $/<tilde-Caret> {
			make Format::Lisp::Directive::Caret.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-C> {
			make Format::Lisp::Directive::C.new
		}
		elsif $/<tilde-D> {
			make Format::Lisp::Directive::D.new
		}
		elsif $/<tilde-E> {
			make Format::Lisp::Directive::E.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-F> {
			make Format::Lisp::Directive::F.new
		}
		elsif $/<tilde-G> {
			make Format::Lisp::Directive::G.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-I> {
			make Format::Lisp::Directive::I.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-O> {
			make Format::Lisp::Directive::O.new
		}
		elsif $/<tilde-Percent> {
			make Format::Lisp::Directive::Percent.new
		}
		elsif $/<tilde-Pipe> {
			make Format::Lisp::Directive::Pipe.new
		}
		elsif $/<tilde-P> {
			make Format::Lisp::Directive::P.new
		}
		elsif $/<tilde-Ques> {
			make Format::Lisp::Directive::Ques.new
		}
		elsif $/<tilde-R> {
			make Format::Lisp::Directive::R.new
		}
		elsif $/<tilde-Semi> {
			make Format::Lisp::Directive::Semi.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-Slash> {
			make Format::Lisp::Directive::Slash.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-Star> {
			make Format::Lisp::Directive::Star.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-S> {
			make Format::Lisp::Directive::S.new
		}
		elsif $/<tilde-Tilde> {
			make Format::Lisp::Directive::Tilde.new
		}
		elsif $/<tilde-T> {
			make Format::Lisp::Directive::T.new
		}
		elsif $/<tilde-Under> {
			make Format::Lisp::Directive::Under.new
		}
		elsif $/<tilde-W> {
			make Format::Lisp::Directive::W.new
		}
		elsif $/<tilde-X> {
			make Format::Lisp::Directive::X.new
		}
	}

	method tilde-Angle( $/ ) {
	}

	method tilde-Brace( $/ ) {
	}

	method tilde-Bracket( $/ ) {
	}

	method tilde-Paren( $/ ) {
	}

#	method Non-Terminal( $/ ) {
#	}

#	method Term( $/ ) {
#	}

	method TOP( $/ ) {
		make 
			#$/<Atom>[0].ast
			$/<Atom>>>.ast
		;
	}
}
