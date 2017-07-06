{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Level2 where

import Debug.Trace

-- -- -- Imports -- -- --
import Data.Either


-- -- -- Data types -- -- --
data Term           = Constant String | Variable String
    deriving (Show, Eq)
type Atom           = (String, Term)
type Clause         = (Atom, [Atom])
type Program        = [Clause]
type Query          = [Atom]
type Substitution   = (Term, Term)

{- Substitution operation in type classes

Usage:
Term/Atom <~ Substitution

-}
class Substitute a where
    (<~) :: a -> Substitution -> a

instance Substitute Term where
    -- Substitute a variable with a Term
    term <~ (original@(Variable _), replacement)
        | term == original  = replacement
        | otherwise         = term
    -- Substitute a constant with a Term
    term <~ (original@(Constant _), replacement)
        | term == original && original == replacement   = replacement
        | original == replacement                       = term
        | otherwise = error "Cannot substitute a constant"

instance Substitute Atom where
    -- Substitute a variable with a Term
    (predicate, term) <~ substitution = (predicate, term <~ substitution)

instance Substitute [Atom] where
     -- Substitue a variable with a Term for all variables in a list of atoms
    atoms <~ substitution = map (<~ substitution) atoms

instance Substitute Clause where
    -- Substitue a variable with a Term for all variables in a clause
    (atom, atoms) <~ substitution 
        = (atom <~ substitution, atoms <~ substitution)


-- -- -- Functions -- -- --

-- Rename function

-- |rename: Renames variables in the source program to fix variable collisions. 
--  Arguments:
--          Program: Program which needs the substitutions.
--          Query: Query which may or may not contain duplicate used variables.
rename :: Program -> Query -> Program
rename program []       = program
rename program query    = init $ foldl renameByAtom (program ++ [(("_query", Constant "a"), query)]) query

-- |renameByAtom: Renames the Program to fix any collision with the Atom.
renameByAtom :: Program -> Atom -> Program
renameByAtom program atom@(_, oldterm@(Variable _)) = 
    map (renameClause (oldterm, (Variable newstr))) program
        where
            newstr = getNewVarName varNames program
            
            renameClause :: Substitution -> Clause -> Clause
            renameClause subst (atom, atoms) = (atom <~ subst, map (<~ subst) atoms)
renameByAtom program _ = program

-- |getNewVarName: Retrieves a string found in [String] not found in any variable in the Program. 
getNewVarName :: [String] -> Program -> String
getNewVarName [] program            = error "No free name found in seed"
getNewVarName (name:seed) program   | elem name (map getstr variables)  
                                        = getNewVarName seed program 
                                    | otherwise
                                        = name
    where
        getstr (Variable str) = str
        getstr (Constant str) = str
        variables   = concat $ map clausevars program
        clausevars  = (\((name, variable),atoms) -> variable : (map atomvars atoms) )
        atomvars    = (\(name, variable) -> variable)

-- |varNames: Generates an infinite list like ["A","B",..,"AA","AB",..]
varNames :: [String] 
varNames = [empty ++ [abc] | empty <- "" : varNames, abc <- ['A'..'Z']]


{- Unify function
Finds a substitution to unify two atoms.

Usage:
unify Atom Atom
-}
unify :: Atom -> Atom -> Substitution
-- Unification of two atoms with a constant term
unify (firstPredicate, firstConstant@(Constant _)) (secondPredicate, secondConstant@(Constant _))
    | firstPredicate /= secondPredicate = error "Cannot unify: nonequal predicates"
    | firstConstant == secondConstant   = (firstConstant, secondConstant)
    | otherwise                         = error "Cannot unify: nonequal constants"
-- Unification of an atom with a variable term and an atom with a constant term
unify (firstPredicate, variable@(Variable _)) (secondPredicate, constant@(Constant _))
    | firstPredicate == secondPredicate = (variable, constant)
    | otherwise                         = error "Cannot unify: nonequal predicates"
-- Unification of an atom with a constant term and an atom with a variable term
unify (firstPredicate, constant@(Constant _)) (secondPredicate, variable@(Variable _))
    | firstPredicate == secondPredicate = (variable, constant)
    | otherwise                         = error "Cannot unify: nonequal predicates"
-- Unification of two atoms with a variable term
unify (firstPredicate, firstVariable@(Variable _)) (secondPredicate, secondVariable@(Variable _))
    | firstPredicate == secondPredicate = (secondVariable, firstVariable)
    | otherwise                         = error "Cannot unify: nonequal predicates"

(<?>) :: Atom -> Atom -> Bool
(firstPredicate, firstConstant@(Constant _)) <?> (secondPredicate, secondConstant@(Constant _))
    | firstPredicate /= secondPredicate = False
    | firstConstant == secondConstant   = True
    | otherwise                         = False
-- Unification of an atom with a variable term and an atom with a constant term
(firstPredicate, (Variable _)) <?> (secondPredicate, (Constant _))
    | firstPredicate == secondPredicate = True
    | otherwise                         = False
-- Unification of an atom with a constant term and an atom with a variable term
(firstPredicate, (Constant _)) <?> (secondPredicate, (Variable _))
    | firstPredicate == secondPredicate = True
    | otherwise                         = False
-- Unification of two atoms with a variable term
(firstPredicate, (Variable _)) <?> (secondPredicate, (Variable _))
    | firstPredicate == secondPredicate = True
    | otherwise                         = False


-- evalOne wrapper function
evalOne :: Program -> Query -> [Either Bool Substitution]
evalOne [] _            = error "Empty program"
evalOne _ []            = error "Empty query"
evalOne program query   | null $ rightsRes      = filter (isLeft) res
                        | otherwise             = rightsRes
    where
        res = eval (rename program query) query
        noConstants = trim res
        rightsRes = filter (isRight) noConstants
        vars = [x | let y = map (snd) query, x@(Variable _) <- y]
        trim :: [Either Bool Substitution] -> [Either Bool Substitution]
        trim []                 = []
        trim (x@(Right (term@(Variable _), _)):xs)  
            | elem term vars    = x : (trim xs)
            | otherwise         = trim xs
        trim (x:xs)             = trim xs

-- eval function
eval :: Program -> Query -> [Either Bool Substitution]
eval [] _ = [Left False]
eval _ [] = [Left True]
eval program query@(queryAtomHead:queryAtoms)
    | null res = [Left False]
    | otherwise = foldr (\(f,s) a -> [f] ++ ( s) ++ a) [] res
    where
        res = [(Right unification, evals)|
            clause@(clauseAtom, clauseAtoms) <- program,
        
--            trace ("query: "++ (show queryAtomHead) ++ " -> " ++ (show queryAtoms) ++ " rule: " ++ (show clauseAtom) ++ " -> "++ (show clauseAtoms))
        
            queryAtomHead <?> clauseAtom,
            let unification = unify queryAtomHead clauseAtom,
            let evals = eval program ((clauseAtoms <~ unification) ++ (queryAtoms <~ unification)), 
            evals /= [Left False]
            ]

-- -- -- Test Data -- -- --
query1 :: Query
query1 = [ -- Desired output unknown
    ("t", Constant "b"),
    ("t", Constant "e")]

program1 :: Program
program1 = [
    (("p", Variable "X"), [("r", Constant "b"), ("s", Variable "X")]),
    (("q", Variable "Y"), [("r", Variable "Y"), ("t", Variable "Y")]),
    (("r", Constant "a"), []),
    (("r", Constant "b"), []),
    (("r", Constant "c"), []),
    (("s", Variable "Z"), [("r", Variable "Z")]),
    (("s", Constant "d"), []),
    (("t", Constant "b"), []),
    (("t", Constant "d"), [])]
