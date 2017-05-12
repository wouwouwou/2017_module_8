lexer grammar OperVocab;

//@header{package pp.block3.cc.antlr.operators;}

NUM		: '0' | (('1'..'9')('0'..'9')*);
BOOL	: 'true' | 'false';
STR		: '"' .*? '"';

HAT 	: '^';
PLUS	: '+';
EQUALS	: '=';
LPAR	: '(';
RPAR	: ')';

WS		: [ \n\r\t] -> skip;