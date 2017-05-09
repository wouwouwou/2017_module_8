import FPPrac.Trees
import Data.Char
import Data.List
import Data.Maybe

---------------------------------------------------------------------------------
-- Example tree with complementary transition functions (because trees in flat
-- text are a real pain!). The MultTree is a generic tree, which can be
-- transformed to exampleTrees which can be used in Exercise 1 and 4.
--
-- TLDR; the exampleTrees are generated from the MultTree.
--
-- EXAMPLE: showTree $ pp1a $ exampleTree1a exampleTree1
--          showTree $ pp1a $ mapTree (^6) $ exampleTree1a exampleTree1
--
-- USAGE: exampleTree1a exampleTree1
--        exampleTree1b exampleTree1
--        exampleTree1c exampleTree1
--        exampleTree1d exampleTree1

data MultTree x  = Nd x [MultTree x]
                        deriving Show

exampleTree1 :: MultTree (Int,Int)
exampleTree1 =
    Nd (50,2) [
        Nd (40,5) [
            Nd (30,2) [],
            Nd (45,2) [
                Nd (60,8) [
                    Nd (61,1) [
                        Nd (62,3) [
                            Nd (63,50) []
                        ]
                    ]
                ]
            ],
            Nd (49,2) []
        ],
        Nd (56,3) [
            Nd (55,2) [],
            Nd (57,3) []
        ],
        Nd (60,8) [
            Nd (61,1) [
                Nd (62,3) [
                    Nd (63,50) []
                ]
            ]
        ],
        Nd (40,5) [
            Nd (30,2) [],
            Nd (45,2) [],
            Nd (49,2) []
        ]
    ]

exampleBinTree :: MultTree (Int, Int) -> BinTree (Int,Int) (Int,Int)
exampleBinTree (Nd x [])           = BinLeaf x
exampleBinTree (Nd x [n])          = BinNode x (exampleBinTree n) (BinLeaf (0,0))
exampleBinTree (Nd x (n:o:ns))     = BinNode x (exampleBinTree n) (exampleBinTree o)

exampleTree2 = BinNode (50,2) (BinNode (40,5) (BinLeaf (30,2)) (BinNode (45,2) (BinNode (61,1) (BinLeaf (62,3)) (BinLeaf ((-1),2))) (BinLeaf (99,3)))) (BinNode (56,3) (BinLeaf (55,2)) (BinLeaf (57,3))) 


--------------------------
--     Excercise 1      --
--------------------------
-- A
data BinTree a b        = BinLeaf b
                        | BinNode a (BinTree a b) (BinTree a b)
                                deriving Show


-- B
data Unit       = Empty
instance Show Unit where 
        show Empty = ""

type Tree1a     = BinTree Int Int 
type Tree1b     = BinTree (Int, Int) (Int, Int)
type Tree1c     = BinTree Int Unit
type Tree4      = BinTree Unit Int

type FSATree    = BinTree Operator (Either Float String)

-- C
pp :: (Show a, Show b) => BinTree a b -> RoseTree
pp (BinLeaf x)          = RoseNode (show x) []
pp (BinNode x a b)      = RoseNode (show x) [pp a, pp b]


--------------------------
--     Excercise 2      --
--------------------------
-- A

--BNF:
-- E -> '(' E O E ')'
-- E -> N
-- N -> [0..9]
-- O -> ['+', '-', '*', '/', '^']

isOperand1 :: Char -> Bool
isOperand1 x = elem x "+=/^*"

data Expr = E | N | O | V
        deriving Show

parseExpr1 :: Expr -> [Char] -> (BinTree Char Int, [Char])
parseExpr1 E (x:xs)     | x == '('      = (BinNode o t1 t2 ,r3)
                        | isDigit x     = (n, rn)
                        | otherwise     = error $ "parse error at start of expression: "++(x:xs)
                                where
                                        (o, ro)    = parseOpr1 r1
                                        (t1, r1)   = parseExpr1 E xs
                                        (t2, r2)   = parseExpr1 E ro
                                        (n, rn)    = parseExpr1 N (x:xs)
                                        r3         | head r2 == ')' = tail r2
                                                   | otherwise = error $ "parse error on closing bracket before: "++ r2
parseExpr1 N (x:xs)     = (BinLeaf $ digitToInt x, xs)

parseOpr1 :: String -> (Char, String) 
parseOpr1 (x:xs)        | isOperand1 x  = (x, xs)
                        | otherwise     = error $ "parse error at start of operand: "++(x:xs)


-- B

--BNF:
-- E -> '(' E O E ')'
-- E -> N
-- E -> V
-- N -> [0..9]
-- O -> ['+', '-', '/', '^']
-- V -> [a..zA..z]

parseExpr2 :: Expr -> [Char] -> (BinTree Char (Either Int Char), [Char])
parseExpr2 E (x:xs)     | x == '('      = (BinNode o t1 t2 ,r3)
                        | isDigit x     = (n, rn)
                        | isAlpha x     = (a, ra)
                        | otherwise     = error $ "parse error at start of expression: "++(x:xs)
                                where
                                        (o, ro)         = parseOpr2 r1
                                        (t1, r1)        = parseExpr2 E xs
                                        (t2, r2)        = parseExpr2 E ro
                                        (n, rn)         = parseExpr2 N (x:xs)
                                        (a, ra)         = parseExpr2 V (x:xs)
                                        r3              | head r2 == ')' = tail r2
                                                        | otherwise = error $ "parse error on closing bracket before: "++r2
parseExpr2 N (x:xs)     = (BinLeaf $ Left $ digitToInt x, xs)
parseExpr2 V (x:xs)     = (BinLeaf $ Right x, xs)

parseOpr2 (x:xs)        | isOperand1 x  = (x, xs)
                        | otherwise     = error $ "parse error at start of operand: "++(x:xs)


--------------------------
--     Excercise 3      --
--------------------------
-- A
data Token = TokNum Float
           | TokId String
           | TokOp Operator
           | TokLB
           | TokRB
           deriving Show

data Operator = Pls | Min | Mlt | Div | Pow
           deriving (Show)

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^<>="

isBracket :: Char -> Bool
isBracket x = x `elem` "()"

toOperator :: Char -> Operator
toOperator '+' = Pls
toOperator '-' = Min
toOperator '*' = Mlt
toOperator '/' = Div
toOperator '^' = Pow
toOperator _ = error ""

data FsaState = Q
              | R
              | S
              deriving (Show, Eq)


-- B
fsaNmbr :: FsaState -> Char -> FsaState
fsaNmbr s x = case s of
  Q | isDigit x -> R
    | x == '~'  -> R
    | otherwise -> Q
  R | isDigit x -> R
    | x == '.'  -> S
    | otherwise -> Q
  S | isDigit x -> S
    | otherwise -> Q

testFsaNmbr = do
  print $ foldl fsaNmbr Q "123"
  print $ foldl fsaNmbr Q "12.34"
  print $ foldl fsaNmbr Q "12.34.56"

fsaIdnt :: FsaState -> Char -> FsaState
fsaIdnt s x = case s of
  Q | isLetter x -> R
    | otherwise  -> Q
  R | isLetter x -> R
    | isDigit  x -> R
    | otherwise  -> Q

testFsaIdnt = do
  print $ foldl fsaIdnt Q "abc"
  print $ foldl fsaIdnt Q "abc123"
  print $ foldl fsaIdnt Q "a1b2c3"

fsaOprt :: FsaState -> Char -> FsaState
fsaOprt s x = case s of
  Q | isOperator x -> R
    | otherwise    -> Q
  R | isOperator x -> R
    | otherwise    -> Q

testFsaOprt = do
  print $ foldl fsaOprt Q "+"
  print $ foldl fsaOprt Q "--"
  print $ foldl fsaOprt Q ">="

fsaBrck :: FsaState -> Char -> FsaState
fsaBrck s x = case s of
  Q | isBracket x -> R
    | otherwise   -> Q
  R -> Q

testFsaBrck = do
  print $ foldl fsaBrck Q "()"

fsaWhiteSpace :: FsaState -> Char -> FsaState
fsaWhiteSpace s x = case s of
  Q | x == ' '  -> R
    | otherwise -> Q
  R | x == ' '  -> R
    | otherwise -> Q


-- C
tokenize :: String -> [Token]
tokenize []     = []
tokenize (c:cs) | isNothing fsa = error "parse error"
                | otherwise = token : tokenize rest
                where
                  fsa = findFSA c
                  (token, rest) = findToken "" (fromJust fsa) Q (c:cs)


findFSA :: Char -> Maybe (FsaState -> Char -> FsaState)
findFSA c | isOperator c = Just fsaOprt
          | isDigit c    = Just fsaNmbr
          | c == '~'     = Just fsaNmbr
          | isAlpha c    = Just fsaIdnt
          | isBracket c  = Just fsaBrck
          | otherwise    = Nothing

findToken :: String -> (FsaState -> Char -> FsaState) -> FsaState -> String -> (Token, String)
findToken res _fsa _s []     = (tok, "")
                             where tok = makeToken res
findToken res fsa  s  (c:cs) | r /= Q = findToken (res++[c]) fsa r cs
                             | otherwise = (tok, c:cs)
                             where
                               r = fsa s c
                               tok = makeToken res

makeToken :: String -> Token
makeToken (c:cs) | isOperator c = TokOp (toOperator c)
                 | isDigit c    = TokNum (read (c:cs))
                 | c == '~'     = TokNum (-1 * read cs)
                 | isAlpha c    = TokId (c:cs)
                 | c == '('     = TokLB
                 | c == ')'     = TokRB
                 | otherwise    = error "empty token"


--------------------------
--     Excercise 4      --
--------------------------
parseExpr4 :: [Token] -> (FSATree, [Token])
parseExpr4 []            = error "empty tree"
parseExpr4 (TokLB:ts)    = (BinNode d t1 t2, tail r3)
                      where
                        (t1, r1) = parse ts
                        (TokOp d:r2) = r1
                        (t2, r3) = parse r2
parseExpr4 (TokNum i:ts) = (BinLeaf (Left i), ts)
parseExpr4 (TokId  s:ts) = (BinLeaf (Right s), ts)

--------------------------
--     Excercise 5      --
--------------------------
eval :: String -> [(String, Float)] -> Float
eval s l = evaluate l $ fst $ parseExpr4 $ tokenize s

evaluate :: [(String, Float)] -> FSATree -> Float
evaluate _ (BinLeaf (Left i))  = i
evaluate l (BinLeaf (Right s)) = find s l
evaluate l (BinNode o t1 t2)   = case o of
  Pls -> evaluate l t1 + evaluate l t2
  Min -> evaluate l t1 - evaluate l t2
  Mlt -> evaluate l t1 * evaluate l t2
  Div -> evaluate l t1 / evaluate l t2
  Pow -> evaluate l t1 ** evaluate l t2

find :: String -> [(String, Float)] -> Float
find q []         = error (q++" not found")
find q ((k,v):ks) | k == q = v
                  | otherwise = find q ks


-- USAGE:   eval (assign (["x"],[24])) (fst $ parseExpr4 (tokenize "(x+3)"))

eval :: (String -> Double) -> FSATree -> Either Double Bool
eval f (BinNode o a1 a2)        | o == "+"      = Left (fromLeft (eval f a1) + fromLeft (eval f a2))
                                | o == "-"      = Left (fromLeft (eval f a1) - fromLeft (eval f a2))
                                | o == "*"      = Left (fromLeft (eval f a1) * fromLeft (eval f a2))
                                | o == "/"      = Left (fromLeft (eval f a1) / fromLeft (eval f a2))
                                | o == "^"      = Left (fromLeft (eval f a1) ** fromLeft (eval f a2))
                                | o == "="      = Right (fromLeft (eval f a1) == fromLeft (eval f a2))
                                | o == "<"      = Right (fromLeft (eval f a1) < fromLeft (eval f a2))
                                | o == ">"      = Right (fromLeft (eval f a1) > fromLeft (eval f a2))
                                | o == "<="     = Right (fromLeft (eval f a1) <= fromLeft (eval f a2))
                                | o == ">="     = Right (fromLeft (eval f a1) >= fromLeft (eval f a2))
                                | o == "/="     = Right (fromLeft (eval f a1) /= fromLeft (eval f a2))
                                | otherwise     = error "operator not recognised"

eval f (BinLeaf x)              = Left (f x)

fromLeft (Left a)               = a
fromLeft (Right _)              = error "Expected Left"
