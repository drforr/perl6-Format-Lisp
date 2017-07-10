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
	has $.at = False;
	has $.colon = False;
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
		make {
			at => ?$/<At>,
			colon => ?$/<Colon>
		}
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
		my $has-at = ?( $/<Tilde-Options><options> and
				$/<Tilde-Options><options>.ast.<at> );
		my $has-colon = ?( $/<Tilde-Options><options> and
				   $/<Tilde-Options><options>.ast.<colon> );
		if $/<not-Tilde> { make $/<not-Tilde>.ast }
		elsif $/<tilde-A> {
			make Format::Lisp::Directive::A.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-Amp> {
			make Format::Lisp::Directive::Amp.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-B> {
			make Format::Lisp::Directive::B.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-Caret> {
			make Format::Lisp::Directive::Caret.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-C> {
			make Format::Lisp::Directive::C.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-D> {
			make Format::Lisp::Directive::D.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-E> {
			make Format::Lisp::Directive::E.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-F> {
			make Format::Lisp::Directive::F.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-G> {
			make Format::Lisp::Directive::G.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-I> {
			make Format::Lisp::Directive::I.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-O> {
			make Format::Lisp::Directive::O.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-Percent> {
			make Format::Lisp::Directive::Percent.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-Pipe> {
			make Format::Lisp::Directive::Pipe.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-P> {
			make Format::Lisp::Directive::P.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-Ques> {
			make Format::Lisp::Directive::Ques.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-R> {
			make Format::Lisp::Directive::R.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-Semi> {
			make Format::Lisp::Directive::Semi.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-Slash> {
			make Format::Lisp::Directive::Slash.new(
				text => $/<tilde-Slash>[0].Str,
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-Star> {
			make Format::Lisp::Directive::Star.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-S> {
			make Format::Lisp::Directive::S.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-Tilde> {
			make Format::Lisp::Directive::Tilde.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-T> {
			make Format::Lisp::Directive::T.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-Under> {
			make Format::Lisp::Directive::Under.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-W> {
			make Format::Lisp::Directive::W.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-X> {
			make Format::Lisp::Directive::X.new(
				at => $has-at,
				colon => $has-colon
			)
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
			$/<Atom>>>.ast
		;
	}
}
