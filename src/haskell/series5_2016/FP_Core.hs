module FP_Core where

import FPPrac.Trees

{-
Extension of CoreIntro.hs:
- instructions as program *in* the processor,
- stack now is list of fixed length,i
- clock added
- program counter + stack pointer added,
- instruction EndProg added,
- update operaton (<~) added,
-}

-- ========================================================================

type Stack  = [Int]

type Heap = [Int]

data Op     = Add | Mul | Sub
            deriving Show

data Instr  = PushConst Int
            | PushAddr Int 
            | Store Int
            | Calc Op
            | PushPC
            | EndRep
            | EndProg
            deriving Show

data Tick = Tick

data Expr = Const Int                   -- for constants
          | Variable Int                -- for variables
          | BinExpr Op Expr Expr        -- for ``binary expressions''

data Stmnt = Assign Int Expr
           | Repeat Expr [Stmnt]

class CodeGen a where
    codeGen :: a -> [Instr]

instance CodeGen Expr where
    codeGen (BinExpr op l r)    = codeGen l ++ codeGen r ++ [Calc op]
    codeGen (Variable i)        = [PushAddr i]
    codeGen (Const n)           = [PushConst n] 

instance CodeGen Stmnt where
    codeGen (Assign i expr)       = codeGen expr ++ [Store i]
    codeGen (Repeat expr stmnts) = codeGen expr ++ [PushPC] ++ concat (map codeGen stmnts) ++ [EndRep]



class ToRose a where
    toRose :: a -> RoseTree
instance ToRose Stmnt where
    toRose (Assign n expr)  = RoseNode ("var " ++ show n ++ " :=") [toRose expr]
instance ToRose Expr where
    toRose (Const n)        = RoseNode (show n) []
    toRose (Variable i)     = RoseNode ("var " ++ show i) []
    toRose (BinExpr op l r) = RoseNode (show op) [toRose l, toRose r]

-- ========================================================================
-- Processor functions

xs <~ (i,a) = take i xs ++ [a] ++ drop (i+1) xs
                -- Put value a on position i in list xs

alu op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)


{-core :: [Instr] -> (Int,Int,Stack) -> Tick -> (Int,Int,Stack)

core instrs (pc,sp,stack) tick =  case instrs!!pc of

        Push n   -> (pc+1, sp+1 , stack <~ (sp,n))

        Calc op  -> (pc+1, sp-1 , stack <~ (sp-2,v))
                 where
                   v = alu op (stack!!(sp-2)) (stack!!(sp-1))

        EndProg  -> (-1, sp, stack)-}

core :: [Instr] -> (Int,Int,Heap,Stack) -> Tick -> (Int,Int,Heap,Stack)

core instrs (pc,sp,heap,stack) tick =  case instrs!!pc of

        PushConst n   -> (pc+1, sp+1 , heap, stack <~ (sp,n))

        PushAddr i  -> (pc+1, sp+1, heap, stack <~ (sp,heap!!i))

        Store i     -> (pc+1, sp-1, heap <~ (i,stack!!(sp-1)), stack)

        Calc op  -> (pc+1, sp-1 , heap, stack <~ (sp-2,v))
                 where
                   v = alu op (stack!!(sp-2)) (stack!!(sp-1))
        
        PushPC   -> (pc+1, sp+1, heap, stack <~ (sp,pc+1))
        
        EndRep   -> (progc, sp', heap, stack <~ (sp-2, stack!!(sp-2)-1))
                 where progc | stack!!(sp-2) > 1   = stack!!(sp-1)
                             | otherwise           = pc+1
                       sp'   | stack!!(sp-2) > 1   = sp
                             | otherwise           = sp-2
                             
        
        EndProg  -> (-1, sp, heap, stack)

-- ========================================================================
-- example Program for expression: (((2*10) + (3*(4+11))) * (12+5))

-- Tree of this expression of type Expr (result of parsing):
expr = BinExpr Mul
          (BinExpr Add
              (BinExpr Mul
                  (Const 2)
                  (Const 10))
              (BinExpr Mul
                  (Const 3)
                  (BinExpr Add
                      (Const 4)
                      (Const 11))))
          (BinExpr Add
              (Const 12)
              (Const 5))

stmnt = Assign 1 (BinExpr Mul
          (BinExpr Add
              (BinExpr Mul
                  (Const 2)
                  (Const 10))
              (BinExpr Mul
                  (Const 3)
                  (BinExpr Add
                      (Const 4)
                      (Const 11))))
          (BinExpr Add
              (Const 12)
              (Const 5)))
stmnt2 = Assign 1 (BinExpr Mul
          (BinExpr Add
              (BinExpr Mul
                  (Variable 1)
                  (Const 10))
              (BinExpr Mul
                  (Const 3)
                  (BinExpr Add
                      (Const 4)
                      (Const 11))))
          (BinExpr Add
              (Const 12)
              (Const 5)))
stmnt3 = Assign 1 (BinExpr Mul
          (BinExpr Add
              (BinExpr Mul
                  (Variable 1)
                  (Const 10))
              (BinExpr Mul
                  (Const 3)
                  (BinExpr Add
                      (Const 4)
                      (Const 11))))
          (BinExpr Add
              (Const 12)
              (Const 5)))
stmnt4 = Repeat expr [stmnt2, stmnt3]

stmnt5 = Repeat (
		BinExpr Add 
		(Const 3) 
		(Const 5)
		) 
		[
		(
		Assign 4 
		(BinExpr Mul
          		(BinExpr Add
              		(BinExpr Mul
                  		(Const 2)
                  		(Const 10))
             			(BinExpr Mul
                  		(Const 3)
                  		(BinExpr Add
                      			(Const 4)
                      			(Const 11))))
          		(BinExpr Add
              			(Const 12)
              			(Const 5)))
		)
		]

stmnt6  = Repeat ( Const 5 ) 
            [
            Repeat (Const 4)
                [Assign 4 (BinExpr Add (Const 1) (Variable 4))]
            ]

-- The program that results in the value of the expression (1105):
{-prog = [ Push 2
       , Push 10
       , Calc Mul
       , Push 3
       , Push 4
       , Push 11
       , Calc Add
       , Calc Mul
       , Calc Add
       , Push 12
       , Push 5
       , Calc Add
       , Calc Mul
       , EndProg
       ]-}

prog = [ PushConst 2
       , PushConst 10
       , Calc Mul
       , PushConst 3
       , PushConst 4
       , PushConst 11
       , Calc Add
       , Calc Mul
       , Calc Add
       , PushConst 12
       , PushConst 5
       , Calc Add
       , Calc Mul
       , Store 5
       , PushConst 111
       , PushAddr 5
       , EndProg
       ]

-- Testing
clock      = repeat Tick
emptyStack = replicate 16 0
emptyHeap  = replicate 8 0
test       = putStr
           $ unlines
           $ map show
           $ takeWhile (\(pc,_,_,_) -> pc /= -1)

           $ scanl (core prog) (0,0,emptyHeap,emptyStack) clock
test2 p    = putStr
           $ unlines
           $ map show
           $ takeWhile (\(pc,_,_,_) -> pc /= -1)

           $ scanl (core p) (0,0,emptyHeap,emptyStack) clock
           


-- ========================================================================
-- New Stuff
-- ========================================================================
{-codeGen :: Expr -> [Instr]
codeGen expr = codeGen' expr ++ [EndProg]

codeGen' :: Expr -> [Instr]
codeGen' (BinExpr op l r)    = codeGen' l ++ codeGen' r ++ [Calc op]
codeGen' (Variable i)        = [PushAddr i]
codeGen' (Const n)           = [PushConst n]

codeGen'' :: Stmnt -> [Instr]
codeGen'' (Assign i expr)   = codeGen' expr ++ [Store i, EndProg]-}

codeGen' :: (CodeGen a) => [a] -> [Instr]
codeGen' a  = concat (map (codeGen) a) ++ [EndProg]

{-codeGen' :: (CodeGen a) => a -> [Instr]
codeGen' a  = codeGen a ++ [EndProg]-}

{-exprToRose :: Expr -> RoseTree
exprToRose (Const n)        = RoseNode (show n) []
exprToRose (Variable i)     = RoseNode ("var " ++ show i) []
exprToRose (BinExpr op l r) = RoseNode (show op) [exprToRose l, exprToRose r]-}

