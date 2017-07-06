module Tokenizer where

import Grammar
import Types
import Data.List
import Data.Char
import FP_ParserGen
import Debug.Trace

-- Scans a String for tokens. Returns a list of tuples,
-- containing the token's alphabet type and the corresponding substring.
tokenizer :: String -> [(Alphabet,String)]
tokenizer [] = []

tokenizer (';':xs)          = (Semi, ";")         : tokenizer xs
tokenizer (',':xs)          = (Comma, ",")        : tokenizer xs

tokenizer input@(x:xs) 
    | isSpace x             = tokenizer xs

    | isPrefixOf "//" input = tokenizer $ endOfLine (input \\ "//")
    | isPrefixOf "/*" input = tokenizer $ endOfBlock (input \\ "/*")

    | elem x "()"           = (Par, [x])          : (tokenizer xs)
    | elem x "{}"           = (Brace, [x])        : (tokenizer xs)
    | word == "procedure"   = (Proc, word)        : (tokenizer wordRest)
    | word == "if"          = (If, word)          : (tokenizer wordRest)
    | word == "else"        = (Else, word)        : (tokenizer wordRest)
    | word == "while"       = (While, word)       : (tokenizer wordRest)
    | word == "fork"        = (Fork, word)        : (tokenizer wordRest)
    | word == "join"        = (Join, word)        : (tokenizer wordRest)
    | word == "global"      = (Global, word)      : (tokenizer wordRest)
    | word == "enum"        = (Enum, word)      : (tokenizer wordRest)
    | word == "print"       = (Print, word)       : (tokenizer wordRest)
    | word == "true"        = (BoolType, word)    : (tokenizer wordRest)
    | word == "false"       = (BoolType, word)    : (tokenizer wordRest)
    | word == "blackjack"   = (BoolType, "true")  : (tokenizer wordRest)
    | word == "hookers"     = (BoolType, "false") : (tokenizer wordRest)
    | word == "int"         = (Type, word)        : (tokenizer wordRest)
    | word == "bool"        = (Type, word)        : (tokenizer wordRest)

    | isPrefixOf "++" input = (OpIncDec, "++")         : tokenizer (input \\ "++")
    | isPrefixOf "--" input = (OpIncDec, "--")         : tokenizer (input \\ "--")
    | isPrefixOf "==" input = (OpEqual, "==")          : tokenizer (input \\ "==")
    | isPrefixOf "!=" input = (OpEqual, "!=")         : tokenizer (input \\ "!=")
    | isPrefixOf "&&" input = (OpAnd, "&&")         : tokenizer (input \\ "&&")
    | isPrefixOf "||" input = (OpOr, "||")          : tokenizer (input \\ "||")
    | isPrefixOf "<>" input = (OpXor, "<>")         : tokenizer (input \\ "<>")
    | isPrefixOf "<=" input = (OpOrd, "<=")         : tokenizer (input \\ "<=")
    | isPrefixOf ">=" input = (OpOrd, ">=")         : tokenizer (input \\ ">=")

    | x ==  '*'             = (OpMulDiv, [x])         : tokenizer xs
    | x ==  '/'             = (OpMulDiv,  [x])         : tokenizer xs
    | x ==  '+'             = (OpPlusMin, [x])         : tokenizer xs
    | x ==  '-'             = (OpPlusMin,  [x])         : tokenizer xs
    | x ==  '!'             = (OpNot,  [x])         : tokenizer xs
    | x ==  '<'             = (OpOrd,   [x])         : tokenizer xs
    | x ==  '>'             = (OpOrd,   [x])         : tokenizer xs
    
    | x ==  '='             = (Ass, [x])          : tokenizer xs
    
    
    | int /= ""             = (IntType, int)      : tokenizer intRest
    | word /= ""            = (Var, word)         : tokenizer wordRest
    | otherwise             = error ("Lexical error at character \"" ++ [x] ++ 
                                  "\", left to scan: \"" ++ xs ++ "\"" )
        where 
            (word,wordRest)     = span isAlphaNumPlus input
            (int,intRest)       = span isNumber input

            endOfLine :: String -> String
            endOfLine []        = []
            endOfLine (x:xs)    | x `elem` "\r\n\f" = xs
                                | otherwise         = endOfLine xs

            endOfBlock :: String -> String
            endOfBlock []       = []
            endOfBlock (x:y:xs) | [x,y] == "*/"     = xs
                                | otherwise         = endOfBlock (y:xs)

-- From series6. Numbers each token with a unique number.
toTokenList :: [(Alphabet, String)] -> [Token]
toTokenList tl = zipWith (\ (x,y) z -> (x,y,z) ) tl [0..]

-- Helper function to accept some special characters in some token types.
isAlphaNumPlus :: Char -> Bool
isAlphaNumPlus char     | char `elem` "_'@#$`~\"?:."  = True
                        | otherwise                   = isAlphaNum char
