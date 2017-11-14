=begin pod

=begin NAME

Format::Lisp - Common Lisp formatter

=end NAME

=begin SYNOPSIS

    my $fl = Format::Lisp.new;
    say $fl.format( "~~,,'~c:~c", ',', 'X' );
    my $func = $fl.formatter( "x~ax" );
    say $fl.formatter-call-to-string( $func, 'X' ); # => xXx

=end SYNOPSIS

=begin DESCRIPTION

Implements the Common Lisp (format) function.

=begin SPEC_DIFFERENCES

In Lisp, ~& only adds a newline if there wasn't a newline on STDOUT previously.

=end SPEC_DIFFERENCES

=end DESCRIPTION

=begin METHODS

=item format( Str $format-string, *@args )

Given a format string and the appropriate (if any) args, return the formatted
output

=end METHODS

=end pod

use Format::Lisp::Grammar;
use Format::Lisp::Actions;

my role Debugging {
}

my role Testing {
}

my role Validating {
}

class Format::Lisp {
	also does Debugging;
	also does Testing;
	also does Validating;

	has $.grammar = Format::Lisp::Grammar.new;
	has $.actions = Format::Lisp::Actions.new;

	method _match( Str $source ) {
		my $parsed = $.grammar.parse( $source );
		$parsed;
	}

	method _parse( Str $source ) {
		my $parsed = $.grammar.parse(
			$source,
			:actions( $.actions )
		);

		$parsed.ast;
	}

	method _format( @directives, @arguments ) {
		my $text = '';
		my $index = 0;
		for @directives -> $directive {
			my $offset = 1;
			if $directive ~~ Format::Lisp::Directive::Angle {
				$text ~= self._format(
					@( $directive.children ),
					@( @arguments[$index] )
				);
			}
			elsif $directive ~~ Format::Lisp::Directive::Bracket {
				if @arguments[$index] == 0 or
				   $directive.at {
					$text ~= self._format(
						@( $directive.children ),
						@( @arguments[$index] )
					);
				}
			}
			$offset = $directive.to-offset(
				$index,
				@arguments[$index],
				@arguments[$index+1],
				@arguments.elems
			);
			$text ~= $directive.to-string(
				@arguments[$index],
				@arguments[$index+1],
				@arguments.elems - $index
			);
			$index += $offset;
		}
		return $text;
	}

	method format( Str $format, **@arguments ) {
		my @directives = self._parse( $format );
		my $text = self._format( @directives, @arguments );
		return $text;
	}

	method formatter( Str $format ) {
		my $fl = self;
		return sub ( *@args ) {
			return $fl.format( $format, |@args );
		}
	}

	method formatter-call-to-string( $formatter, **@arguments ) {
		return $formatter( @arguments );
	}
}
