grammar ClassFixed;

c   : p 'class' ID x y;
p   : 'public'
    | ;
x   : 'extends' ID
    | ;
y   : 'implements' ID i
    | ;
i   : ',' ID i
    | ;

ID  : [a-z]+;
WS  : [ \t\r\n]+ -> skip;