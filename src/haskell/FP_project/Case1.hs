module Case1 where

--import Data.List
-- import LPTypes

data Term       = Atom String
    deriving (Show, Eq)
type Clause     = (Term, [Term])
type Program    = [Clause]
type Query      = [Term]



--import LPEvaluator

evalProp :: Program -> Query -> Bool
evalProp [] _           = False
evalProp _ []           = True
evalProp program (q:qs) | null [ True | (c, cs) <- program, {- trace traceLine True, -} c == q, evalProp program (cs ++ qs) ] 
                                = False
                        | otherwise 
                                = True
    {- where         
         traceLine = ("query: "++ (show q) ++ " ++ " ++ (show qs) ++ " rule: " ++ (show c) ++ " -> "++ (show cs)) -}

program :: Program
program = [(Atom "a0", []),
           (Atom "a1", []),
           (Atom "a2", []),
           (Atom "b0", [Atom "a0", Atom "a1"]),
           (Atom "b1", [Atom "a1", Atom "a2"]),
           (Atom "b2", [Atom "a0", Atom "a1", Atom "d"]),
           (Atom "c0", [Atom "b0", Atom "b1"]),
           (Atom "c1", [Atom "b0", Atom "b1", Atom "b2"])
           ]

query0 = [Atom "c0"]
query1 = [Atom "c1"]



test = do
  print (evalProp program query0)
  print (evalProp program query1)
