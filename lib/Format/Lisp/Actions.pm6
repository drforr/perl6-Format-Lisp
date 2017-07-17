=begin pod

=begin NAME

Format::Lisp::Actions - Actions for Common Lisp format strings

=end NAME

=begin DESCRIPTION

=end DESCRIPTION

=end pod

role Stringify {
	has $.text;
}

role Nested {
	has @.children;
}

class Format::Lisp::Text {
	also does Stringify;
	method to-string( $_argument, $next, $remaining ) {
		return $.text;
	}
}

class Format::Lisp::Directive {
	has $.at = False;
	has $.colon = False;

	method print-case( $text ) {
		if $*PRINT-CASE {
			given $*PRINT-CASE {
				when 'upcase' {
					return uc( $text );
				}
				when 'downcase' {
					return lc( $text );
				}
				when 'capitalize' {
					return tc( lc( $text ) );
				}
			}
		}
		return $text;
	}
}

class Format::Lisp::Directive::A is Format::Lisp::Directive {
	has $.mincol = 0;
	has $.colinc = 1;
	has $.minpad = 0;
	has $.padchar = ' ';

	method pad( $out, $at, $mincol, $colinc, $minpad, $padchar ) {
		my $padding = '';
		while $mincol > $out.chars + $padding.chars {
			$padding ~= $padchar x $colinc;
		}
		while $minpad > $padding.chars {
			$padding ~= $padchar x $minpad;
		}

		return $padding ~ $out if $at;
		return $out ~ $padding;
	}

	method to-string( $_argument, $next, $remaining ) {
		my $argument = $_argument;

		my $mincol = $.mincol;
		if $mincol eq 'next' {
			$mincol = $argument // 0;
			$argument = $next;
		}
		elsif $mincol eq 'remaining' {
			$mincol = $remaining;
		}

		my $colinc = $.colinc;
		if $colinc eq 'next' {
			$colinc = $argument // 1;
			$argument = $next;
		}
		elsif $colinc eq 'remaining' {
			$colinc = $remaining;
		}

		my $minpad = $.minpad;
		if $minpad eq 'next' {
			$minpad = $argument // 0;
			$argument = $next;
		}
		elsif $minpad eq 'remaining' {
			$minpad = $remaining;
		}

		my $padchar = $.padchar;
		if $padchar eq 'next' {
			$padchar = $argument // ' ';
			$argument = $next;
		}
		elsif $padchar eq 'remaining' {
			$padchar = $remaining;
		}

		my $out = $argument;
		if $argument ~~ List {
			if $.colon {
				$out = '(NIL)';
			}
			else {
				$out = '(NIL)'; # Sigh.
			}
		}
		elsif !$argument {
			if $.colon {
				$out = '()';
			}
			else {
				$out = 'NIL';
			}
		}
		$out = self.print-case( $out );

		return self.pad(
			$out, $.at, $mincol, $colinc, $minpad, $padchar
		);
	}
}
class Format::Lisp::Directive::Amp is Format::Lisp::Directive { }
class Format::Lisp::Directive::Angle is Format::Lisp::Directive {
	also does Nested;
	has $.trailing-colon = False;
}
class Format::Lisp::Directive::B is Format::Lisp::Directive { }
class Format::Lisp::Directive::Brace is Format::Lisp::Directive {
	also does Nested;
	has $.trailing-colon = False;
}
class Format::Lisp::Directive::Bracket is Format::Lisp::Directive {
	also does Nested;
}
class Format::Lisp::Directive::Caret is Format::Lisp::Directive { }
class Format::Lisp::Directive::C is Format::Lisp::Directive { }
class Format::Lisp::Directive::D is Format::Lisp::Directive { }
class Format::Lisp::Directive::Dollar is Format::Lisp::Directive { }
class Format::Lisp::Directive::E is Format::Lisp::Directive { }
class Format::Lisp::Directive::F is Format::Lisp::Directive { }
class Format::Lisp::Directive::G is Format::Lisp::Directive { }
class Format::Lisp::Directive::I is Format::Lisp::Directive { }
class Format::Lisp::Directive::O is Format::Lisp::Directive { }
class Format::Lisp::Directive::Paren is Format::Lisp::Directive {
	also does Nested;
}
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
class Format::Lisp::Directive::S is Format::Lisp::Directive {
	has $.mincol = 0;
	has $.colinc = 1;
	has $.minpad = 0;
	has $.padchar = ' ';

	method pad( $out, $at, $mincol, $colinc, $minpad, $padchar ) {
		my $padding = '';
		while $mincol > $out.chars + $padding.chars {
			$padding ~= $padchar x $colinc;
		}
		while $minpad > $padding.chars {
			$padding ~= $padchar x $minpad;
		}

		return $padding ~ $out if $at;
		return $out ~ $padding;
	}

	method to-string( $_argument, $next, $remaining ) {
		my $argument = $_argument;

		my $mincol = $.mincol;
		if $mincol eq 'next' {
			$mincol = $argument // 0;
			$argument = $next;
		}
		elsif $mincol eq 'remaining' {
			$mincol = $remaining;
		}

		my $colinc = $.colinc;
		if $colinc eq 'next' {
			$colinc = $argument // 1;
			$argument = $next;
		}
		elsif $colinc eq 'remaining' {
			$colinc = $remaining;
		}

		my $minpad = $.minpad;
		if $minpad eq 'next' {
			$minpad = $argument // 0;
			$argument = $next;
		}
		elsif $minpad eq 'remaining' {
			$minpad = $remaining;
		}

		my $padchar = $.padchar;
		if $padchar eq 'next' {
			$padchar = $argument // ' ';
			$argument = $next;
		}
		elsif $padchar eq 'remaining' {
			$padchar = $remaining;
		}

		my $out = $argument;
		if $argument ~~ List {
			if $.colon {
				$out = '(NIL)';
			}
			else {
				$out = '(NIL)'; # Sigh.
			}
		}
		elsif !$argument {
			if $.colon {
				$out = '()';
			}
			else {
				$out = 'NIL';
			}
		}
		$out = self.print-case( $out );

		return self.pad(
			$out, $.at, $mincol, $colinc, $minpad, $padchar
		);
	}
}
class Format::Lisp::Directive::Tilde is Format::Lisp::Directive { }
class Format::Lisp::Directive::T is Format::Lisp::Directive { }
class Format::Lisp::Directive::Under is Format::Lisp::Directive { }
class Format::Lisp::Directive::W is Format::Lisp::Directive { }
class Format::Lisp::Directive::X is Format::Lisp::Directive { }

class Format::Lisp::Actions {
	method not-Tilde( $/ ) {
		make Format::Lisp::Text.new(
			text => ~$/
		)
	}

	method Default( $/ ) {
		make 'remaining'
	}

	method Character( $/ ) {
		make ~$/
	}

	method V( $/ ) { make 'next' }

	method signed-integer( $/ ) { make +$/ }

	method value( $/ ) {
		make	$/<Default>.ast
		||	$/<Character>.ast
		||	$/<V>.ast
		||	$/<signed-integer>.ast
	}

	method options( $/ ) {
		make {
			at => ?$/<At>,
			colon => ?$/<Colon>
		}
	}

	method value-comma( $/ ) {
		make $/<value>.ast
	}

	method Tilde-Options( $/ ) {
		make {
			at => ?( $/<options>.ast.<at> ),
			colon => ?( $/<options>.ast.<colon> ),
		}
	}

	method Atom( $/ ) {
		my $has-at = $/<Tilde-Options>.ast.<at>;
		my $has-colon = $/<Tilde-Options>.ast.<colon>;
		my @arguments;
		@arguments.append( $/<Tilde-Options><value-comma>>>.ast ) if
			$/<Tilde-Options><value-comma>;
		@arguments.append( $/<Tilde-Options><value>.ast ) if
			$/<Tilde-Options><value> or
			$/<Tilde-Options><value-comma>;
		if $/<not-Tilde> { make $/<not-Tilde>.ast }
		elsif $/<tilde-A> {
			make Format::Lisp::Directive::A.new(
				at => $has-at,
				colon => $has-colon,
				mincol => @arguments[0] // 0,
				colinc => @arguments[1] // 1,
				minpad => @arguments[2] // 0,
				padchar => @arguments[3] // ' '
			)
		}
		elsif $/<tilde-Amp> {
			make Format::Lisp::Directive::Amp.new(
				at => $has-at,
				colon => $has-colon,
			)
		}
		elsif $/<tilde-Angle> {
			my $has-trailing-colon =
				?( $/<tilde-Angle><tilde-CAngle><Tilde-Options><options> and
				   $/<tilde-Angle><tilde-CAngle><Tilde-Options><options>.ast.<colon> );
			my @children;
			@children.append( $/<tilde-Angle><TOP><Atom>>>.ast ) if
				$/<tilde-Angle><TOP><Atom>;
			make Format::Lisp::Directive::Angle.new(
				at => $has-at,
				colon => $has-colon,
				trailing-colon => $has-trailing-colon,
				children => @children
			)
		}
		elsif $/<tilde-B> {
			make Format::Lisp::Directive::B.new(
				at => $has-at,
				colon => $has-colon
			)
		}
		elsif $/<tilde-Brace> {
			my $has-trailing-colon =
				?( $/<tilde-Angle><tilde-CBrace><Tilde-Options><options> and
				   $/<tilde-Angle><tilde-CBrace><Tilde-Options><options>.ast.<colon> );
			my @children;
			@children.append( $/<tilde-Brace><TOP><Atom>>>.ast ) if
				$/<tilde-Brace><TOP><Atom>;
			make Format::Lisp::Directive::Brace.new(
				at => $has-at,
				colon => $has-colon,
				trailing-colon => $has-trailing-colon,
				children => @children
			)
		}
		elsif $/<tilde-Bracket> {
			my $has-trailing-colon =
				?( $/<tilde-Angle><tilde-CBracket><Tilde-Options><options> and
				   $/<tilde-Angle><tilde-CBracket><Tilde-Options><options>.ast.<colon> );
			my @children;
			@children.append( $/<tilde-Bracket><TOP><Atom>>>.ast ) if
				$/<tilde-Bracket><TOP><Atom>;
			make Format::Lisp::Directive::Bracket.new(
				at => $has-at,
				colon => $has-colon,
				trailing-colon => $has-trailing-colon,
				children => @children
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
		elsif $/<tilde-Dollar> {
			make Format::Lisp::Directive::Dollar.new(
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
		elsif $/<tilde-Paren> {
			my $has-trailing-colon =
				?( $/<tilde-Angle><tilde-CParen><Tilde-Options><options> and
				   $/<tilde-Angle><tilde-CParen><Tilde-Options><options>.ast.<colon> );
			my @children;
			@children.append( $/<tilde-Paren><TOP><Atom>>>.ast ) if
				$/<tilde-Paren><TOP><Atom>;
			make Format::Lisp::Directive::Paren.new(
				at => $has-at,
				colon => $has-colon,
				trailing-colon => $has-trailing-colon,
				children => @children
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
				colon => $has-colon,
				mincol => @arguments[0] // 0,
				colinc => @arguments[1] // 1,
				minpad => @arguments[2] // 0,
				padchar => @arguments[3] // ' '
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

	method TOP( $/ ) {
		make 
			$/<Atom>>>.ast
		;
	}
}
