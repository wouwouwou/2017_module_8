lexer grammar cc14;

LA : 'L''a'+ ' '*;
LALA : LA LA ;
fragment LI: 'L''i' ' '*;
LALALALI : LA LA LA LI ;
WS    : [\t\r\n]+ -> skip ; 		// At least one whitespace char; don't make token