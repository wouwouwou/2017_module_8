grammar Expr;

goal	: expr EOF;
expr	: PARENTHOPEN expr PARENTHCLOSE 	#exprPar
		| <assoc=right> expr EXP expr 		#exprExp
		| expr MULT expr 					#expMul
		| expr (PLUS|MIN) expr				#expPM
		| num								#expNum;
num		: DIG								#numPos
		| MIN num							#numNeg;

DIG : '0' | ('1'..'9')('0'..'9')*;
PLUS : '+';
MIN : '-';
EXP : '^';
MULT : '*';
PARENTHOPEN : '(';
PARENTHCLOSE : ')';

// ignore whitespace
WS : [ \t\n\r] -> skip;

// everything else is a typo
TYPO : [a-zA-Z]+;