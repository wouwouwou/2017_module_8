lexer grammar cc13;

SENTENCE: '\"' (~'"'|QUOT)* '\"';
fragment QUOT: '\"\"';