module ASTBuilder where

import FPPrac.Trees
import Types
import Data.Maybe
import Data.List

pTreeToAst :: ParseTree -> AST
pTreeToAst (PNode Program l)
    = ASTProgram (map pTreeToAst l) ([],[],[],[])
pTreeToAst (PNode Global (typ:var:[]))
    = ASTGlobal (getAlphabet (getStr typ)) (pTreeToAst var) Nothing ([],[],[],[])
pTreeToAst (PNode Global (typ:var:(PLeaf (Ass,_,_)):expr:[]))
    = ASTGlobal (getAlphabet (getStr typ)) (pTreeToAst var) (Just (pTreeToAst expr)) ([],[],[],[])
pTreeToAst (PNode Enum (_:enum:(PLeaf (Ass,_,_)):(PLeaf (Brace,_,_)):values))
    = ASTEnum (getStr enum) (map pTreeToAst values) ([],[],[],[])
pTreeToAst (PNode Proc (pid:args_expr))
        = ASTProc (getStr pid) (makeAstArg $ init args_expr) expr ([],[],[], [])
            where
                expr = pTreeToAst $ last args_expr

                makeAstArg :: [ParseTree] -> [AST]
                makeAstArg [] = []
                makeAstArg [x] = error "This Proc is incorrectly parsed"
                makeAstArg (x:y:xs) = (ASTArg (pTreeToAst x) (pTreeToAst y) ([],[],[], [])) : (makeAstArg xs)

-- Basic Parse Nodes
pTreeToAst node@(PNode Type _)
    = ASTType (getStr node) ([],[],[],[])

pTreeToAst node@(PNode Var _)
    = ASTVar (getStr node) ([],[],[],[])

---- STATEMENTS ----
-- DeclStat (no assign) 
pTreeToAst (PNode Stat (typ@(PNode Type _):var:[]))
    = ASTDecl (getAlphabet (getStr typ)) (pTreeToAst var) Nothing ([],[],[],[])
-- DeclStat (assign)
pTreeToAst (PNode Stat (typ@(PNode Type _):var:(PLeaf (Ass,_,_)):expr:[]))
    = ASTDecl (getAlphabet (getStr typ)) (pTreeToAst var) (Just $ pTreeToAst expr) ([],[],[],[])
-- If statement
pTreeToAst (PNode Stat ((PLeaf (If,_,_)):expr:stat1:[]))
    = ASTIf (pTreeToAst expr) (pTreeToAst stat1) Nothing ([],[],[],[])
-- If statment with else
pTreeToAst (PNode Stat ((PLeaf (If,_,_)):expr:stat1:_:stat2:[]))
    = ASTIf (pTreeToAst expr) (pTreeToAst stat1) (Just $ pTreeToAst stat2) ([],[],[],[])
-- While statement
pTreeToAst (PNode Stat ((PLeaf (While,_,_)):expr:stat:[]))
    = ASTWhile (pTreeToAst expr) (pTreeToAst stat) ([],[],[],[])
-- Fork statement
pTreeToAst (PNode Stat ((PLeaf (Fork,_,_)):pid:args_expr))
    = ASTFork (getStr pid) (map pTreeToAst args_expr) ([],[],[],[])
-- Join statement
pTreeToAst (PNode Stat ((PLeaf (Join,_,_)):[]))
    = ASTJoin ([],[],[],[])
-- Call statement
pTreeToAst (PNode Stat (pid@(PNode Pid _):args_expr))
    = ASTCall (getStr pid) (map pTreeToAst args_expr) ([],[],[],[])
-- Expression statement
pTreeToAst (PNode Stat (expr@(PNode Expr _):[]))
    = ASTExpr (pTreeToAst expr) Nothing ([],[],[],[])
-- Block statement
pTreeToAst (PNode Stat ((PLeaf (Brace,_,_)):stats))
    = ASTBlock (map pTreeToAst stats) ([],[],[],[])
-- Print statement
pTreeToAst (PNode Stat ((PLeaf (Print,_,_)):exprs))
    = ASTPrint (map pTreeToAst exprs) ([],[],[],[])
-- Assign statement
pTreeToAst (PNode Stat (var:(PLeaf (Ass,_,_)):expr:[]))
        = ASTAss (pTreeToAst var) (pTreeToAst expr) Nothing ([],[],[],[])

---- EXPRESSIONS ----
pTreeToAst (PNode Expr (or@(PNode OR _):[]))
        = pTreeToAst or
pTreeToAst (PNode Expr (or@(PNode OR _):expr'@(PNode Expr' (op@(PLeaf (OpOr, _, _)):_)):_))
        = ASTOp (pTreeToAst or) (getStr op) (pTreeToAst expr') Nothing ([],[],[],[])
pTreeToAst (PNode Expr' (_:or@(PNode OR _):[]))
        = pTreeToAst or
pTreeToAst (PNode Expr' (_:or@(PNode OR _):(expr'@(PNode Expr' (op@(PLeaf (OpOr, _, _)):_)):_)))
        = ASTOp (pTreeToAst or) (getStr op) (pTreeToAst expr') Nothing ([],[],[],[])

pTreeToAst (PNode OR (xor@(PNode XOr _):[]))
        = pTreeToAst xor
pTreeToAst (PNode OR (xor@(PNode XOr _):or'@(PNode OR' (op@(PLeaf (OpXor, _, _)):_)):_))
        = ASTOp (pTreeToAst xor) (getStr op) (pTreeToAst or') Nothing ([],[],[],[])
pTreeToAst (PNode OR' (_:xor@(PNode XOr _):[]))
        = pTreeToAst xor
pTreeToAst (PNode OR' (_:xor@(PNode XOr _):(or'@(PNode OR' (op@(PLeaf (OpXor, _, _)):_)):_)))
        = ASTOp (pTreeToAst xor) (getStr op) (pTreeToAst or') Nothing ([],[],[],[])

pTreeToAst (PNode XOr (and@(PNode AND _):[]))
        = pTreeToAst and
pTreeToAst (PNode XOr (and@(PNode AND _):xor'@(PNode XOr' (op@(PLeaf (OpAnd, _, _)):_)):_))
        = ASTOp (pTreeToAst and) (getStr op) (pTreeToAst xor') Nothing ([],[],[],[])
pTreeToAst (PNode XOr' (_:and@(PNode AND _):[]))
        = pTreeToAst and
pTreeToAst (PNode XOr' (_:and@(PNode AND _):(xor'@(PNode XOr' (op@(PLeaf (OpAnd, _, _)):_)):_)))
        = ASTOp (pTreeToAst and) (getStr op) (pTreeToAst xor') Nothing ([],[],[],[])

pTreeToAst (PNode AND (equal@(PNode EQUAL _):[]))
        = pTreeToAst equal
pTreeToAst (PNode AND (equal@(PNode EQUAL _):and'@(PNode AND' (op@(PLeaf (OpEqual, _, _)):_)):_))
        = ASTOp (pTreeToAst equal) (getStr op) (pTreeToAst and') Nothing ([],[],[],[])
pTreeToAst (PNode AND' (_:equal@(PNode EQUAL _):[]))
        = pTreeToAst equal
pTreeToAst (PNode AND' (_:equal@(PNode EQUAL _):(and'@(PNode AND' (op@(PLeaf (OpEqual, _, _)):_)):_)))
        = ASTOp (pTreeToAst equal) (getStr op) (pTreeToAst and') Nothing ([],[],[],[])

pTreeToAst (PNode EQUAL (ord@(PNode Ord _):[]))
        = pTreeToAst ord
pTreeToAst (PNode EQUAL (ord@(PNode Ord _):equal'@(PNode EQUAL' (op@(PLeaf (OpOrd, _, _)):_)):_))
        = ASTOp (pTreeToAst ord) (getStr op) (pTreeToAst equal') Nothing ([],[],[],[])
pTreeToAst (PNode EQUAL' (_:ord@(PNode Ord _):[]))
        = pTreeToAst ord
pTreeToAst (PNode EQUAL' (_:ord@(PNode Ord _):(equal'@(PNode EQUAL' (op@(PLeaf (OpOrd, _, _)):_)):_)))
        = ASTOp (pTreeToAst ord) (getStr op) (pTreeToAst equal') Nothing ([],[],[],[])

pTreeToAst (PNode Ord (term@(PNode Term _):[]))
        = pTreeToAst term
pTreeToAst (PNode Ord (term@(PNode Term _):ord'@(PNode Ord' (op@(PLeaf (OpPlusMin, _, _)):_)):_))
        = ASTOp (pTreeToAst term) (getStr op) (pTreeToAst ord') Nothing ([],[],[],[])
pTreeToAst (PNode Ord' (_:term@(PNode Term _):[]))
        = pTreeToAst term
pTreeToAst (PNode Ord' (_:term@(PNode Term _):(ord'@(PNode Ord' (op@(PLeaf (OpPlusMin, _, _)):_)):_)))
        = ASTOp (pTreeToAst term) (getStr op) (pTreeToAst ord') Nothing ([],[],[],[])

pTreeToAst (PNode Term (fact@(PNode Factor _):[]))
        = pTreeToAst fact
pTreeToAst (PNode Term (fact@(PNode Factor _):term'@(PNode Term' (op@(PLeaf (OpMul, _, _)):_)):_))
        = ASTOp (pTreeToAst fact) (getStr op) (pTreeToAst term') Nothing ([],[],[],[])
pTreeToAst (PNode Term' (_:fact@(PNode Factor _):[]))
        = pTreeToAst fact
pTreeToAst (PNode Term' (_:fact@(PNode Factor _):term'@(PNode Term' (op@(PLeaf (OpMul, _, _)):_)):_))
        = ASTOp (pTreeToAst fact) (getStr op) (pTreeToAst term') Nothing ([],[],[],[])

-- Parentheses expression
pTreeToAst (PNode Factor (expr@(PNode Expr _):[]))
        = pTreeToAst expr
-- Unary prefix
pTreeToAst (PNode Factor (op@(PNode PreUnary _):expr:[]))
        = ASTPreUnary (getStr op) (pTreeToAst expr) Nothing ([],[],[],[])
-- Variable expression
pTreeToAst (PNode Factor (var@(PNode Var _):[]))
        = ASTVar (getStr var) ([],[],[],[])
-- IntType expression
pTreeToAst (PNode Factor (intType@(PNode IntType _):[]))
        = ASTInt (getStr intType) ([],[],[],[])
-- BoolType expression
pTreeToAst (PNode Factor (boolType@(PNode BoolType _):[]))
        = ASTBool (getStr boolType) ([],[],[],[])

astToRoseDebug :: AST -> RoseTree
astToRoseDebug (ASTProgram asts (f,g,v,_))
    = RoseNode ("program" ++ " -> " ++ (show f) ++ (show g) ++ (show (getDeepestScope v))) (map astToRoseDebug asts)
astToRoseDebug (ASTGlobal typeStr ast Nothing (f,g,v,_))
    = RoseNode ("global " ++ (getTypeStr typeStr) ++ " -> " ++ (show f) ++ (show g) ++ (show (getDeepestScope v))) [(astToRoseDebug ast)]
astToRoseDebug (ASTGlobal typeStr ast1 (Just ast2) (f,g,v,_)) 
    = RoseNode ("global " ++ (getTypeStr typeStr) ++ " -> "++ (show f) ++ (show g) ++ (show (getDeepestScope v))) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTEnum enum asts (f,g,v,enums)) -- map (\(e,es) -> "(" ++ (e) ++ "," ++ (intercalate "," es) ++ ")") enums)
    = RoseNode ("enum " ++ enum ++ " -> " ++ (show f) ++ (show g) ++ (show (getDeepestScope v)) ++ show enums) $ map astToRoseDebug asts
astToRoseDebug (ASTProc str asts ast (f,g,v,e))
    = RoseNode ("procedure " ++ str ++ " -> " ++ (show f) ++ (show g) ++ (show (getDeepestScope v)) ++ show e) $ map astToRoseDebug $ asts ++ [ast]
astToRoseDebug (ASTArg ast1 ast2 (f,g,v,_))
    = RoseNode ("arg" ++ " -> " ++ (show f) ++ (show g) ++ (show (getDeepestScope v))) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTBlock asts (f,g,v,e))
    = RoseNode ("block" ++ " -> " ++ (show f) ++ (show g) ++ (show ({-getDeepestScope-} v)) ++ show e) $ map astToRoseDebug asts
astToRoseDebug (ASTDecl typeStr ast Nothing (f,g,v,_))
    = RoseNode (getTypeStr typeStr ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) [(astToRoseDebug ast)]
astToRoseDebug (ASTDecl typeStr ast1 (Just ast2) (f,g,v,_))
    = RoseNode (getTypeStr typeStr ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTIf ast1 ast2 Nothing (f,g,v,_))
    = RoseNode ("if" ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTIf ast1 ast2 (Just ast3) (f,g,v,_))
    = RoseNode ("if" ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) $ map astToRoseDebug [ast1, ast2, ast3]
astToRoseDebug (ASTWhile ast1 ast2 (f,g,v,_))
    = RoseNode ("while" ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTFork str asts (f,g,v,_))
    = RoseNode ("fork " ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) $ map astToRoseDebug asts
astToRoseDebug (ASTJoin (f,g,v,_))
    = RoseNode ("join" ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) []
astToRoseDebug (ASTCall str asts (f,g,v,_))
    = RoseNode ("call " ++ " -> " ++ (show f) ++ (show g) ++ (show (getDeepestScope v))) $ map astToRoseDebug asts
astToRoseDebug (ASTPrint asts (f,g,v,_))
    = RoseNode ("print " ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) $ map astToRoseDebug asts
astToRoseDebug (ASTExpr ast typeStr (f,g,v,_))
    = RoseNode ("expr " ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) $ [(astToRoseDebug ast)]
astToRoseDebug (ASTAss ast1 ast2 typeStr (f,g,v,_))
    = RoseNode ("= -> " ++ (show f) ++ (show g) ++ (show (getDeepestScope v))) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTVar str (f,g,v,_))
    = RoseNode (str ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) []
astToRoseDebug (ASTInt str (f,g,v,_))
    = RoseNode (str ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) []
astToRoseDebug (ASTBool str (f,g,v,_))
    = RoseNode (str ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) []
astToRoseDebug (ASTType str (f,g,v,_))
    = RoseNode (str ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) []
astToRoseDebug (ASTOp ast1 str ast2 typeStr (f,g,v,_))
    = RoseNode (str ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTPreUnary str ast typeStr (f,g,v,_))
    = RoseNode (str ++ " -> " {-++ (show f) ++ (show g)-} ++ (show (getDeepestScope v))) [(astToRoseDebug ast)]

getDeepestScope :: [[VariableType]] -> [VariableType]
getDeepestScope [] = []
getDeepestScope [[]] = []
getDeepestScope (v:_) = v


astToRose :: AST -> RoseTree
astToRose (ASTProgram asts _) 
    = RoseNode "program" (map astToRose asts)
astToRose (ASTGlobal typeStr ast Nothing _)
    = RoseNode ("global " ++ (getTypeStr typeStr)) [(astToRose ast)]
astToRose (ASTGlobal typeStr ast1 (Just ast2) _)
    = RoseNode ("global " ++ (getTypeStr typeStr)) $ map astToRose [ast1, ast2]
astToRose (ASTEnum enum asts _)
    = RoseNode ("enum " ++ enum) $ map astToRose asts
astToRose (ASTProc str asts ast _)
    = RoseNode ("procedure " ++ str) $ map astToRose $ asts ++ [ast]
astToRose (ASTArg ast1 ast2 _)
    = RoseNode "arg" $ map astToRose [ast1, ast2]
astToRose (ASTBlock asts _)
    = RoseNode "block" $ map astToRose asts
astToRose (ASTDecl typeStr ast Nothing _)
    = RoseNode (getTypeStr typeStr) [(astToRose ast)]
astToRose (ASTDecl typeStr ast1 (Just ast2) _)
    = RoseNode (getTypeStr typeStr) $ map astToRose [ast1, ast2]
astToRose (ASTIf ast1 ast2 Nothing _)
    = RoseNode "if" $ map astToRose [ast1, ast2]
astToRose (ASTIf ast1 ast2 (Just ast3) _)
    = RoseNode "if" $ map astToRose [ast1, ast2, ast3]
astToRose (ASTWhile ast1 ast2 _)
    = RoseNode "while" $ map astToRose [ast1, ast2]
astToRose (ASTFork str asts _)
    = RoseNode ("fork " ++ str) $ map astToRose asts
astToRose (ASTJoin _)
    = RoseNode "join" []
astToRose (ASTCall str asts _)
    = RoseNode ("call " ++ str) $ map astToRose asts
astToRose (ASTPrint asts _)
    = RoseNode "print" $ map astToRose asts
astToRose (ASTExpr ast _ _)
    = RoseNode "expr" [(astToRose ast)]
astToRose (ASTAss ast1 ast2 _ _)
    = RoseNode "=" $ map astToRose [ast1, ast2]
astToRose (ASTVar str _)
    = RoseNode str []
astToRose (ASTInt str _)
    = RoseNode str []
astToRose (ASTBool str _)
    = RoseNode str []
astToRose (ASTType str _)
    = RoseNode str []
astToRose (ASTOp ast1 str ast2 _ _)
    = RoseNode str $ map astToRose [ast1, ast2]
astToRose (ASTPreUnary str ast _ _)
    = RoseNode str [(astToRose ast)]



getStr :: ParseTree -> String
getStr (PLeaf (_,str,_))    = str
getStr (PNode Var [x])      = getStr x
getStr (PNode Pid [x])      = getStr x
getStr (PNode BoolType [x]) = getStr x
getStr (PNode IntType [x])  = getStr x
getStr (PNode Op [x])       = getStr x
getStr (PNode PreUnary [x]) = getStr x
getStr (PNode Type [x])     = getStr x
getStr (PNode Expr _)       = error "Cannot return the string of an expression this way."
getStr a                    = error (show a)

getAlphabet :: String -> Alphabet
getAlphabet "int"   = IntType
getAlphabet "bool"  = BoolType
getAlphabet "enum"  = EnumType
getAlphabet _       = error "Type not recognised."

getTypeStr :: Alphabet -> String
getTypeStr IntType  = "int"
getTypeStr BoolType = "bool"
getTypeStr EnumType = "enum"
getTypeStr _        = error "Not a valid type in: getStr :: Alphabet -> String"
