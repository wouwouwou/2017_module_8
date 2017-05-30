lexer grammar printf;

//FORMATSTRING : '"' (STRING | FORMAT)* '"';
//STRING : ~('"%\\');
GENERAL         : '%' ARGINDEX? GFLAG* WIDTH? PRECISION? GCONVERSION;
CHARACTER       : '%' ARGINDEX? CFLAG* WIDTH? PRECISION? CCONVERSION;
INTEGRAL        : '%' ARGINDEX? IFLAG* WIDTH? PRECISION? ICONVERSION;
FLOATINGPOINT   : '%' ARGINDEX? FFLAG* WIDTH? PRECISION? FCONVERSION;

fragment ARGINDEX           : NUMBER '$';

fragment GFLAG              : '-#';
fragment CFLAG              : '-';
fragment IFLAG              : '-#+ 0,(';
fragment FFLAG              : '-#+ 0,(';



fragment WIDTH              : POSINT;

fragment PRECISION          : '.' NUMBER;

fragment GCONVERSION        : CONVERT_BOOL
                            | CONVERT_HEX
                            | CONVERT_STR;

fragment CCONVERSION        : CONVERT_UNICODE;

fragment ICONVERSION        : CONVERT_DECIMAL
                            | CONVERT_OCTAL
                            | CONVERT_INT_HEX;

fragment FCONVERSION        : CONVERT_SCIENTIFIC
                            | CONVERT_FLOAT
                            | CONVERT_HYBRID
                            | CONVERT_SCIENTIFIC_A;

fragment NUMBER             : '0'
                            | POSINT;

fragment POSINT             : [1-9] DIGIT*;

fragment DIGIT              : [0-9];
fragment CONVERT_BOOL       : 'bB';
fragment CONVERT_HEX        : 'hH';
fragment CONVERT_STR        : 'sS';
fragment CONVERT_UNICODE    : 'cC';
fragment CONVERT_DECIMAL    : 'd';
fragment CONVERT_OCTAL      : 'o';
fragment CONVERT_INT_HEX    : 'xX';
fragment CONVERT_SCIENTIFIC : 'eE';
fragment CONVERT_FLOAT      : 'f';
fragment CONVERT_HYBRID     : 'gG';
fragment CONVERT_SCIENTIFIC_A : 'aA';