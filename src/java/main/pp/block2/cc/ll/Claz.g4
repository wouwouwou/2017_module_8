lexer grammar Claz;

C   : P CLASS ID X Y;
fragment P   : PUBLIC
    | ;
fragment X   : EXTENDS ID
    | ;
fragment Y   : IMPLEMENTS I ID
    | ;
fragment I   : ID COMMA I
    | ;

CLASS: 'class';
PUBLIC : 'public';
EXTENDS : 'extends';
IMPLEMENTS : 'implements';
COMMA: ',';

ID  : [a-z]+;
WS  : [ \t\r\n]+ -> skip;