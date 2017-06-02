module Case2 where

import LPTypes
import LPEvaluator
import Data.Either

program :: Program
program = [(Atom "p" [Const "a"], []),
           (Atom "p" [Const "b"], []),
           (Atom "p" [Const "c"], []),
           (Atom "q" [Const "a"], []),
           (Atom "q" [Const "b"], []),
           (Atom "r" [Var "X"], [Atom "p" [Var "X"], Atom "q" [Var "X"]])
           ]

query0 = [Atom "r" [Const "a"]]
query1 = [Atom "r" [Const "b"]]
query2 = [Atom "r" [Const "c"]]
query3 = [Atom "r" [Var "X"]]

test = do
 print (evalOne program query0)
 print (evalOne program query1)
 print (evalOne program query2)
 print (evalOne program query3)