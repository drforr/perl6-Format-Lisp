=begin pod

=begin NAME

Format::Lisp - Common Lisp formatter

=end NAME

=begin SYNOPSIS

    my $fl = Format::Lisp.new;
    print $fl.format( "~~,,'~c:~c", ',', 'X' );

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

	method process-directive( $directive, *@arguments ) {
	}

	method format( Str $format, **@arguments ) {
		my @directives = self._parse( $format );
		my $text = '';
		my $index = 0;
		for @directives -> $directive {
			if $directive ~~ Format::Lisp::Directive::Star {
#`(
				my $offset = $directive.to-string(
					@arguments[$index],
					@arguments[$index+1],
					@arguments.elems - $index
				);
)
if $directive.at {
if $directive.colon {
if $directive.n ~~ Real {
warn "01";
}
elsif $directive.n ~~ Str {
if $directive.n eq 'remaining' {
warn "02";
}
elsif $directive.n eq 'next' {
warn "03";
}
}
else {
warn "04";
}
}
else {
if $directive.n ~~ Real {
#warn "11";
	$index = $directive.n;
}
elsif $directive.n ~~ Str {
if $directive.n eq 'remaining' {
warn "12";
}
elsif $directive.n eq 'next' {
#warn "13";
	if @arguments[$index] ~~ Real {
#warn "13a";
		$index = @arguments[$index];
	}
	else {
#warn "13b";
		$index = 0;
	}
}
}
else {
#warn "14";
	$index = 0;
}
}
}
else {
if $directive.colon {
if $directive.n ~~ Real {
#warn "21";
$index -= $directive.n;
}
elsif $directive.n ~~ Str {
if $directive.n eq 'remaining' {
warn "22";
}
elsif $directive.n eq 'next' {
#warn "23";
if @arguments[$index] ~~ Real {
if @arguments[$index] == 2 {
#warn "23a";
$index--;
# Do nothing
}
else {
#warn "23b";
$index = @arguments[$index+1];
}
}
else {
#warn "23c";
}
}
}
else {
#warn "24";
$index--;
}
}
else {
if $directive.n ~~ Real {
#warn "31";
# Skip
}
elsif $directive.n ~~ Str {
if $directive.n eq 'remaining' {
warn "32";
}
elsif $directive.n eq 'next' {
#warn "33";
if $index+1 < @arguments.elems - 1 {
$index = ( @arguments[$index] // 1 ) + @arguments[$index+1];
}
else {
$index = @arguments[$index+1];
}
}
}
else {
#warn "34";
$index++;
}
}
}
			}
			else {
				$text ~= $directive.to-string(
					@arguments[$index],
					@arguments[$index+1],
					@arguments.elems - $index
				);
				$index++;
			}
		}
		return $text;
	}

	method formatter( Str $format ) {
		my $fl = self;
		return sub {
			return $fl.format( $format, @_ );
		}
	}

	method formatter-call-to-string( $formatter, **@arguments ) {
		return $formatter( @arguments );
	}
}
