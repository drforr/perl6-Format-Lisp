our $CHAR-CODE-LIMIT is export = 0x10000;
our @standard-chars is export =
	'a'..'z',
	'A'..'Z',
	'0'..'9',
	split( '', Q{~!@#$%^&*()_+|\\=-`{}[]:\";'<>?,./} ),
	"\n"
;
