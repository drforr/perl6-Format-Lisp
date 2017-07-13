=begin pod

=begin NAME

Format::Lisp - Common Lisp formatter

=end NAME

=begin SYNOPSIS

    my $fl = Format::Lisp.new;
    print $fl.format( "~~,,'~c:~c", ',', 'X' );

=end SYNOPSIS

=begin DESCRIPTION

=end DESCRIPTION

=begin DEBUGGING

=end DEBUGGING

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

	method process-directive( $directive, *@arguments ) {
	}

	method format( Str $format, *@arguments ) {
		my @directives = self._parse( $format );
		my $text = '';
		for @directives.kv -> $index, $_ {
			$text ~= $_.to-string( @arguments[$index] );
		}
		return $text;
	}
}
