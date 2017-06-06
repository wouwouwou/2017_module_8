module Level1 where


-- Imports
import Debug.Trace


-- Data types

data Term       = Atom String
    deriving (Show, Eq)
type Clause     = (Term, [Term])
type Program    = [Clause]
type Query      = [Term]


-- Test programs

program1 = [(Atom "a", []), 
            (Atom "b", [Atom "c", Atom "d"]), 
            (Atom "b", [Atom "c", Atom "e"]), 
            (Atom "c", []),
            (Atom "d", [Atom "f"]),
            (Atom "e", [])]
            
program2 = [(Atom "a", []), 
            (Atom "b", [Atom "c", Atom "d"]), 
            (Atom "b", [Atom "c", Atom "e"]), 
            (Atom "c", [Atom "d"]),
            (Atom "d", [Atom "f"]),
            (Atom "e", [])]
            
query1   = [Atom "a", Atom "b"]

-- Functions
evalProp :: Program -> Query -> Bool
evalProp [] _           = False
evalProp _ []           = True
evalProp program (q:qs) | res == [] = False
                        | otherwise = True
    where 
        res = [ True | (c, cs) <- program, {- trace traceLine True, -} c == q, evalProp program (cs ++ qs) ]
        
        {- traceLine = ("query: "++ (show q) ++ " ++ " ++ (show qs) ++ " rule: " ++ (show c) ++ " -> "++ (show cs)) -}
        
        
