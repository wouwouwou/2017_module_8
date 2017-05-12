grammar Oper;

import OperVocab;

t 	: t HAT t		#hat
	| t PLUS t		#plus
	| t EQUALS t	#equals
	| LPAR t RPAR	#parentheses
	| NUM			#num
	| BOOL			#bool
	| STR			#str
	;

