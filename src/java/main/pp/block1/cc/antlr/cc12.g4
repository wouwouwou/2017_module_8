lexer grammar cc12;

IDENTIFIER: (LETTER CHARACTER CHARACTER CHARACTER CHARACTER CHARACTER);
fragment CHARACTER: UPPERCASE | LOWERCASE | DIGIT;
fragment LETTER: UPPERCASE | LOWERCASE;
fragment UPPERCASE : 'A'..'Z';
fragment LOWERCASE : 'a'..'z';
fragment DIGIT : '0'..'9' ;			//All digits
WS    : [ \t\r\n]+ -> skip ; 		// At least one whitespace char; don't make token

//Answer on question 3: if you omit fragment, the token type changes. Each character is seen as a token, instead of the 6 character identifier.