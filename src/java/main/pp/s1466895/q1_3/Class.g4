grammar Class;

c   : p 'class' ID x y;
p   : 'public'
    | ;
x   : 'extends' ID
    | ;
y   : 'implements' i ID
    | ;
i   : ID ',' i
    | ;

ID  : [a-z]+;
WS  : [ \t\r\n]+ -> skip;