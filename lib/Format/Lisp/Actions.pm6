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

class Format::Lisp::Directive::A { also does Stringify; }
class Format::Lisp::Directive::Amp { also does Stringify; }
class Format::Lisp::Directive::B { also does Stringify; }
class Format::Lisp::Directive::Caret { also does Stringify; }
class Format::Lisp::Directive::C { also does Stringify; }
class Format::Lisp::Directive::D { also does Stringify; }
class Format::Lisp::Directive::E { also does Stringify; }
class Format::Lisp::Directive::F { also does Stringify; }
class Format::Lisp::Directive::G { also does Stringify; }
class Format::Lisp::Directive::I { also does Stringify; }
class Format::Lisp::Directive::O { also does Stringify; }
class Format::Lisp::Directive::Percent { also does Stringify; }
class Format::Lisp::Directive::Pipe { also does Stringify; }
class Format::Lisp::Directive::P { also does Stringify; }
class Format::Lisp::Directive::Ques { also does Stringify; }
class Format::Lisp::Directive::R { also does Stringify; }
class Format::Lisp::Directive::Semi { also does Stringify; }
class Format::Lisp::Directive::Slash { also does Stringify; }
class Format::Lisp::Directive::Star { also does Stringify; }
class Format::Lisp::Directive::S { also does Stringify; }
class Format::Lisp::Directive::Tilde { also does Stringify; }
class Format::Lisp::Directive::T { also does Stringify; }
class Format::Lisp::Directive::Under { also does Stringify; }
class Format::Lisp::Directive::W { also does Stringify; }
class Format::Lisp::Directive::X { also does Stringify; }

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
	}

	method Tilde-Options( $/ ) {
		make $/<value-comma>>>.ast
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
			make Format::Lisp::Directive::A.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-Amp> {
			make Format::Lisp::Directive::Amp.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-B> {
			make Format::Lisp::Directive::B.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-Caret> {
			make Format::Lisp::Directive::Caret.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-C> {
			make Format::Lisp::Directive::C.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-D> {
			make Format::Lisp::Directive::D.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-E> {
			make Format::Lisp::Directive::E.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-F> {
			make Format::Lisp::Directive::F.new(
				options => $/<Tilde-Options>
			)
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
			make Format::Lisp::Directive::O.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-Percent> {
			make Format::Lisp::Directive::Percent.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-Pipe> {
			make Format::Lisp::Directive::Pipe.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-P> {
			make Format::Lisp::Directive::P.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-Ques> {
			make Format::Lisp::Directive::Ques.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-R> {
			make Format::Lisp::Directive::R.new(
				options => $/<Tilde-Options>
			)
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
			make Format::Lisp::Directive::S.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-Tilde> {
			make Format::Lisp::Directive::Tilde.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-T> {
			make Format::Lisp::Directive::T.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-Under> {
			make Format::Lisp::Directive::Under.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-W> {
			make Format::Lisp::Directive::W.new(
				options => $/<Tilde-Options>
			)
		}
		elsif $/<tilde-X> {
			make Format::Lisp::Directive::X.new(
				options => $/<Tilde-Options>
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
		my @x;
		@x.append( $/<Atom>>>.ast ) if $/<Atom>;
		@x.append( $/<Term>>>.ast ) if $/<Term>;
		make @x;
	}
}
