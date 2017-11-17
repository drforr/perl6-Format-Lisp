=begin pod

=begin NAME

Format::Lisp::Actions - Actions for Common Lisp format strings

=end NAME

=begin DESCRIPTION

=end DESCRIPTION

=end pod

my role Nested {
	has @.children;
}

my role Padded {
	has $.mincol = 0;
	has $.padchar = ' ';
}

my role Number-Like {
	also does Padded;
	has $.commachar = ',';
	has $.comma-interval = 3;

	has $.argument;

	method _pad( $_out, $mincol, $padchar ) {
		my $out = self._print-case( $_out );

		return ( $padchar x ( $mincol - $out.chars ) ) ~ $out
			if $mincol > $out.chars;

		return $out;
	}

	method _to-number( $mincol, $padchar, $commachar, $comma-interval ) {
		my $out = $.argument;
		my $chars-to-commify = $out.chars;
		$chars-to-commify-- if $out ~~ /^\-/;
		if $.colon and $chars-to-commify > $comma-interval {
			my $commas-to-add =
				$chars-to-commify / $comma-interval;
			$commas-to-add-- if $comma-interval == 1;
			for 0 .. $commas-to-add - 1 -> $x {
				my $inset = ( $comma-interval * ( $x + 1 )) + $x;
				$out.substr-rw( *-$inset, 0 ) = $commachar;
			}
		}
		$out = '+' ~ $out if $.at and $out > 0;

		return self._pad( $out, $mincol, $padchar );
	}

	method _formatter( $next, $remaining ) { !!! }

	method _get-attribute( $remaining, $default, $attribute ) {
		return self._attribute( $remaining, $default, $attribute );
	}
	method _adjust-argument( $next, $attribute ) {
		$!argument = self._argument( $next, $attribute );
	}

	method to-string( $_argument, $next, $remaining ) {
		$!argument = $_argument;

		my $mincol = self._get-attribute( $remaining, 0, $.mincol );
		self._adjust-argument( $next, $.mincol );

		my $padchar = self._get-attribute( $remaining, ' ', $.padchar );
		self._adjust-argument( $next, $.padchar );

		my $commachar =
			self._get-attribute( $remaining, ',', $.commachar );
		self._adjust-argument( $next, $.commachar );

		my $comma-interval =
			self._get-attribute( $remaining, 3, $.comma-interval );
		self._adjust-argument( $next, $.comma-interval );

		$!argument = self._formatter( $next, $remaining );

		return self._to-number(
			$mincol, $padchar, $commachar, $comma-interval
		);
	}
}

my role String-Like {
	also does Padded;
	has $.colinc = 1;
	has $.minpad = 0;

	has $.argument;

	method _pad( $mincol, $colinc, $minpad, $padchar ) {
		my $out = self._print-case( $.argument );
		my $padding = '';
		if $minpad > 0 {
			$padding = $padchar x $minpad;
		}
		if $mincol > $out.chars + $padding.chars {
			my $remainder = $mincol - $out.chars - $padding.chars;
			my $pads = ( $remainder / $colinc ).ceiling * $colinc;
			$padding ~= $padchar x $pads;
		}

		return $padding ~ $out if $.at;
		return $out ~ $padding;
	}

	method _get-attribute( $remaining, $default, $attribute ) {
		return self._attribute( $remaining, $default, $attribute );
	}
	method _adjust-argument( $next, $attribute ) {
		$!argument = self._argument( $next, $attribute );
	}

	method to-string( $_argument, $next, $remaining ) {
		$!argument = $_argument;

		my $mincol = self._get-attribute( $remaining, 0, $.mincol );
		self._adjust-argument( $next, $.mincol );

		my $colinc = self._get-attribute( $remaining, 1, $.colinc );
		self._adjust-argument( $next, $.colinc );

		my $minpad = self._get-attribute( $remaining, 0, $.minpad );
		self._adjust-argument( $next, $.minpad );

		my $padchar = self._get-attribute( $remaining, ' ', $.padchar );
		self._adjust-argument( $next, $.padchar );

		$!argument = self._get-nil( $.argument, $.argument );

		return self._pad( $mincol, $colinc, $minpad, $padchar );
	}
}

class Format::Lisp::Text {
	has $.text;

	method to-string( $argument, $next, $remaining ) {
		return $.text;
	}

	method to-offset( $index, $arg, $next, $elems ) {
		return 0;
	}
}

class Format::Lisp::Directive {
	has $.at = False;
	has $.colon = False;

	method _attribute( $remaining, $default, $attribute ) {
		return $.argument // $default if $attribute eq 'next';
		return $remaining if $attribute eq 'remaining';
		return $attribute;
	}
	method _argument( $next, $attribute ) {
		return $next if $attribute eq 'next';
		return $.argument if $attribute eq 'remaining';
		return $.argument;
	}

	method _print-case( $text ) {
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

	method _get-nil( $argument, $out ) {
		if $argument ~~ List {
			return '(NIL)' if $.colon;
			return '(NIL)'; # Sigh.
		}
		elsif !$argument {
			return '()' if $.colon;
			return 'NIL';
		}
		return $out;
	}

	method to-string( $_argument, $next, $remaining ) {
		return '';
	}

	method to-offset( $index, $arg, $next, $elems ) {
		return 1;
	}
}

class Format::Lisp::Directive::A is Format::Lisp::Directive {
	also does String-Like;
}

class Format::Lisp::Directive::Amp is Format::Lisp::Directive {
	has $.n = 0;
	has $.argument;

	method to-string( $argument, $next, $remaining ) {
		$!argument = $argument;
		my $n = self._attribute( $remaining, 0, $.n );

		return qq{\n} x $n;
	}
}

class Format::Lisp::Directive::Angle is Format::Lisp::Directive {
	also does Nested;
	has $.trailing-colon = False;

	method to-offset( $index, $arg, $next, $elems ) {
		return 1;
	}
}

class Format::Lisp::Directive::B is Format::Lisp::Directive {
	also does Number-Like;

	method _formatter( $next, $remaining ) {
		return sprintf "%b", $.argument;
	}
}

class Format::Lisp::Directive::Brace is Format::Lisp::Directive {
	also does Padded;
	also does Nested;
	has $.trailing-colon = False;
}

class Format::Lisp::Directive::Bracket is Format::Lisp::Directive {
	also does Padded;
	also does Nested;
	has $.trailing-colon = False;

	method to-offset( $index, $arg, $next, $elems ) {
		return 1;
	}
	method postprocess( $text, $index, @arguments ) {
		return $text if @arguments[$index] == 0 or self.at;
		return '';
	}
}

class Format::Lisp::Directive::Caret is Format::Lisp::Directive {
}

class Format::Lisp::Directive::C is Format::Lisp::Directive {
	method _to-character-name( $character ) {
		my %character-names =
			qq{ } => 'Space',
			qq{\x08} => 'Backspace',
			qq{\x09} => 'Tab',
			qq{\x7f} => 'Rubout',
			qq{\x0a} => 'Linefeed',
			qq{\x0d} => 'Return',
			qq{\x0f} => 'Page'
		;

		return %character-names{$character}
			if %character-names{$character};
		return $character;
	}

	method to-string( $_argument, $next, $remaining ) {
		my $argument = $_argument;

		$argument = self._to-character-name( $argument ) if $.colon;

		return self._print-case( $argument );
	}
}

class Format::Lisp::Directive::D is Format::Lisp::Directive {
	also does Number-Like;

	method _formatter( $next, $remaining ) {
		return sprintf "%d", $.argument;
	}
}

class Format::Lisp::Directive::Dollar is Format::Lisp::Directive { }

class Format::Lisp::Directive::E is Format::Lisp::Directive { }

class Format::Lisp::Directive::F is Format::Lisp::Directive {
	also does Number-Like;

	method _formatter( $next, $remaining ) {
		my $value = sprintf "%f", $.argument;
		$value = $value.substr( 0, $.mincol ) if $.mincol;
		return $value;
	}
}

class Format::Lisp::Directive::G is Format::Lisp::Directive { }

class Format::Lisp::Directive::I is Format::Lisp::Directive { }

class Format::Lisp::Directive::O is Format::Lisp::Directive {
	also does Number-Like;

	method _formatter( $next, $remaining ) {
		return sprintf "%o", $.argument;
	}
}

class Format::Lisp::Directive::Paren is Format::Lisp::Directive {
	also does Nested;
	method postprocess( $text, $index, @arguments ) {
		if self.at {
			return uc( $text ) if self.colon;
			return tclc( $text );
		}
		if self.colon {
		}
		return lc( $text );
	}
}

class Format::Lisp::Directive::Percent is Format::Lisp::Directive {
	has $.n = 1;
	has $.argument;

	method to-string( $argument, $next, $remaining ) {
		$!argument = $argument;
		my $n = self._attribute( $remaining, 0, $.n );

		return qq{\n} x $n;
	}
}

class Format::Lisp::Directive::Pipe is Format::Lisp::Directive { 
}

class Format::Lisp::Directive::P is Format::Lisp::Directive {
	method to-string( $_argument, $next, $remaining ) {
		# XXX Of course, this is heavily language-dependent.
		return 's' if $_argument and $_argument != 1;
		return '';
	}
}

class Format::Lisp::Directive::Ques is Format::Lisp::Directive {
}

class Format::Lisp::Directive::R is Format::Lisp::Directive {
	also does Number-Like;
	has $.radix = 10;

	method _formatter( $next, $remaining ) {
		my $radix = self._get-attribute( $remaining, 10, $.radix );
		my $argument = self._adjust-argument( $next, $.radix );

		return $argument.base( $radix );
	}
}

class Format::Lisp::Directive::Semi is Format::Lisp::Directive { }

class Format::Lisp::Directive::Slash is Format::Lisp::Directive {
	has $.text;

	method to-string( $argument, $next, $remaining ) {
		return $argument;
	}
}

class Format::Lisp::Directive::Star is Format::Lisp::Directive {
	has $.n = Nil;

	method to-offset( $index, $argument, $next, $elems ) {
		if $.at {
			return $.n - $index if $.n ~~ Real;
			if $.n ~~ Str {
				if $.n eq 'remaining' {
warn "12";
					return -$index;
				}
				return ( $argument // 0 ) - $index if
					$.n eq 'next';
				return -$index;
			}
			return -$index;
		}
		else {
			if $.colon {
				return -$.n if $.n ~~ Real;
				if $.n ~~ Str {
					if $.n eq 'remaining' {
warn "22";
					}
					elsif $.n eq 'next' {
						if $argument ~~ Real {
							return -1 if
								$argument == 2;
							return $next - $index;
						}
						return 0;
					}
				}
				return -1;
			}
			else {
				if $.n ~~ Str {
					if $.n eq 'remaining' {
warn "32";
						return 0;
					}
					elsif $.n eq 'next' {
						if $index + 1 < $elems - 1 {
							return ( $argument // 1 ) + $next - $index;
						}
						return $next - $index;
					}
					return 0;
				}
				return 0 if $.n ~~ Real;
				return 1;
			}
		}
	}
}

class Format::Lisp::Directive::S is Format::Lisp::Directive {
	also does String-Like;
}

class Format::Lisp::Directive::Tilde is Format::Lisp::Directive {
	method to-offset( $index, $arg, $next, $elems ) {
		return 0;
	}

	method to-string( $_argument, $next, $remaining ) {
		return '~';
	}
}

class Format::Lisp::Directive::T is Format::Lisp::Directive {
}

class Format::Lisp::Directive::Under is Format::Lisp::Directive {
}

class Format::Lisp::Directive::W is Format::Lisp::Directive { }

class Format::Lisp::Directive::X is Format::Lisp::Directive {
	also does Number-Like;

	method _formatter( $next, $remaining ) {
		return sprintf "%x", $.argument;
	}
}

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
				n => @arguments[0] // 0
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
				colon => $has-colon,
				mincol => @arguments[0] // 0,
				padchar => @arguments[1] // ' ',
				commachar => @arguments[2] // ',',
				comma-interval => @arguments[3] // 3
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
				colon => $has-colon,
				mincol => @arguments[0] // 0,
				padchar => @arguments[1] // ' ',
				commachar => @arguments[2] // ',',
				comma-interval => @arguments[3] // 3
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
				colon => $has-colon,
				mincol => @arguments[0] // 0,
				padchar => @arguments[1] // ' ',
				commachar => @arguments[2] // ',',
				comma-interval => @arguments[3] // 3
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
				colon => $has-colon,
				mincol => @arguments[0] // 0,
				padchar => @arguments[1] // ' ',
				commachar => @arguments[2] // ',',
				comma-interval => @arguments[3] // 3
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
				colon => $has-colon,
				n => @arguments[0] // 1
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
				colon => $has-colon,
				radix => @arguments[0] // 10,
				mincol => @arguments[1] // 0,
				padchar => @arguments[2] // ' ',
				commachar => @arguments[3] // ',',
				comma-interval => @arguments[4] // 3
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
				colon => $has-colon,
				n => @arguments[0] // Nil
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
				colon => $has-colon,
				mincol => @arguments[0] // 0,
				padchar => @arguments[1] // ' ',
				commachar => @arguments[2] // ',',
				comma-interval => @arguments[3] // 3
			)
		}
	}

	method TOP( $/ ) {
		make 
			$/<Atom>>>.ast
		;
	}
}
