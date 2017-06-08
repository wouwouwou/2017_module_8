{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Case3 where

import Data.Either
import Debug.Trace
import Data.List
import Data.Char
import Data.List.Split
import System.IO

--------------------------
--    import LPTypes    --
--------------------------
data Term      = Const String
               | Var   String
                  deriving (Show, Eq)
type Atom      = (String, [Term])
type Program   = [Clause]
type Clause    = (Atom, [Atom])
type Query     = [Atom]
type Substitution = (Term, Term)

class Substitute a where
    (<~) :: a -> Substitution -> a

instance Substitute Term where
  (Var a)   <~ (Var b, c) | a == b = c | otherwise = Var a
  (Var _)   <~ (Const _, _)                        = error ""
  (Const a) <~ _                                   = Const a

instance Substitute [Term] where
    terms <~ subst = map (<~ subst) terms

instance Substitute Atom where
    (pred, term) <~ subst = (pred, term <~ subst)

instance Substitute [Atom] where
    atoms <~ subst = map (<~ subst) atoms

instance Substitute Clause where
    (atom, atoms) <~ subst
        = (atom <~ subst, atoms <~ subst)

instance Substitute Program where
    program <~ subst = map (<~ subst) program


--------------------------
--  import LPEvaluator  --
--------------------------

-- -- Rename -- --
-- -- Renames the variables in a given program such that the       -- --
-- -- variables can not conflict with the used variables in a      -- --
-- -- given query. Temporarily adds the query as a clause to the   -- --
-- -- program, after which it uses foldl and renamePerAtom to      -- --
-- -- give the conflicting variables a unique name.                -- --
------------------------------------------------------------------------
rename :: Program -> Query -> Program
rename program query = foldl (<~) program (getSubstitutions program query)

-- -- GetSubstitutions -- --
------------------------------------------------------------------------
getSubstitutions :: Program -> Query -> [Substitutions]
getSubstitutions program query = zip (nub $ intersect programvars queryvars) (map (\x -> Var x) newvars)
    where
         programvars = [x | ]
         queryvars   = [x | ]
         newvars     = generateVars varlist (program ++ [(("_query", Const "a"), query)])

-- -- RenamePerAtom -- --
-- -- Renames / substitutes the given atom in every clause of the  -- --
-- -- given program  by a newly generated variable, only if the    -- --
-- -- given atom is a variable. Gets the new variable from the     -- --
-- -- generateVar funtion.                                         -- --
------------------------------------------------------------------------
renamePerAtom :: Program -> Atom -> Program
renamePerAtom program (_, oldterm@(Var _)) =
  map (<~ (oldterm, (Var newvars))) program
   where
     newvars = generateVars varlist program

renamePerAtom program _ = program

-- -- Generatevars -- --
-- -- Returns a list of variables which can be used as unique new  -- --
-- -- variables in a Program. Parameters:                          -- --
-- -- (Possible Variable Names, Program)                           -- --
-- -- Example: newvars = generateVars varlist program              -- --
------------------------------------------------------------------------
generateVars :: [String] -> Program -> [String]
generateVars [] program               = error "No free variable found!"
generateVars (newvar:seed) program    | elem newvar (map getstr vars)
                                          = generateVar seed program
                                     | otherwise
                                          = newvar : generateVar seed program
  where
     getstr (Var str)      = str
     getstr (Const str)    = str
     vars                  = concat $ map clausevars program
     clausevars            = (\((_, var), ats) -> var : (map atomvars ats))
     atomvars              = (\ (_, var)       -> var)


-- -- Varlist -- --
-- -- Returns an infinite list of possible variable names.         -- --
-- -- This list looks like: ["A", "B", ..., "AA", "AB", ...]       -- --
------------------------------------------------------------------------
varlist :: [String]
varlist = [prefix ++ [alph] | prefix <- "" : varlist, alph <- ['A'..'Z']]


-- -- Unify -- --
-- -- Returns a substitution which can be used to unify two atoms. -- --
-- -- Example: (p, Const a) -> (p, Var X) -> (Var X, Const a)      -- --
------------------------------------------------------------------------
unify :: Atom -> Atom -> Substitution
unify (firstpred,  firstconst@(Const _))
      (secondpred, secondconst@(Const _))
           | firstpred  /= secondpred   = error "Can not unify: predicates not equal!"
           | firstconst == secondconst  = (firstconst, secondconst)
           | otherwise                  = error "Can not unify: constants not equal!"

unify (firstpred,  var@(Var _))
      (secondpred, const@(Const _))
           | firstpred == secondpred    = (var, const)
           | otherwise                  = error "Can not unify: predicates not equal!"

unify (firstpred,  const@(Const _))
      (secondpred, var@(Var _))
           | firstpred == secondpred    = (var, const)
           | otherwise                  = error "Can not unify: predicates not equal!"

unify (firstpred,  firstvar@(Var _))
      (secondpred, secondvar@(Var _))
           | firstpred == secondpred    = (secondvar, firstvar)
           | otherwise                  = error "Can not unify: predicates not equal!"


-- -- Unifyable infix Operator <?> -- --
-- -- Returns True if two atoms can be unified by substitution.    -- --
-- -- Example: (p, Const a) -> (p, Var X) -> True                  -- --
------------------------------------------------------------------------
(<?>) :: Atom -> Atom -> Bool
(firstpred, firstconst@(Const _)) <?> (secondpred, secondconst@(Const _))
           | firstpred  /= secondpred   = False
           | firstconst == secondconst  = True
           | otherwise                  = False

(firstpred, var@(Var _))          <?> (secondpred, const@(Const _))
           | firstpred == secondpred    = True
           | otherwise                  = False

(firstpred, const@(Const _))      <?> (secondpred, var@(Var _))
           | firstpred == secondpred    = True
           | otherwise                  = False

(firstpred, firstvar@(Var _))     <?> (secondpred, secondvar@(Var _))
           | firstpred == secondpred    = True
           | otherwise                  = False


-- -- evalOne -- --
-- -- Evaluates a query for a program after renaming. After        -- --
-- -- evaluating, filters and trims the result to either a list    -- --
-- -- with a single boolean, or a list of substitutions for which  -- --
-- -- the query becomes true.                                      -- --
------------------------------------------------------------------------
evalOne :: Program -> Query -> [Either Bool Substitution]
evalOne [] _           = error "The program is empty!"
evalOne _ []           = error "The query is empty!"
evalOne program query  | null $ rightRes    = filter (isLeft) res
                       | otherwise          = rightRes
   where
       rightRes = filter (isRight) noConstants
       noConstants = trim res
       res = eval (rename program query) query

       trim :: [Either Bool Substitution] -> [Either Bool Substitution]
       trim [] = []
       trim (x@(Right (term@(Var _), _)):xs)
         | elem term vars       = x : (trim xs)
         | otherwise            = trim xs
       trim (x:xs)              = trim xs

       vars = [x | let y = map (snd) query, x@(Var _) <- y]


-- -- eval -- --
-- -- The evaluating part of evalOne. Looks if the queried atom is -- --
-- -- unifiable. If so, adds the corresponding substitution to a   -- --
-- -- list. After that, it a recursive call will be done for the   -- --
-- -- remaining clause- and query-atoms. In the end, this          -- --
-- -- function will return a list with booleans and substitutions. -- --
------------------------------------------------------------------------
eval :: Program -> Query -> [Either Bool Substitution]
eval [] _ = [Left False]
eval _ [] = [Left True]
eval program (qAtom:qAtoms)
    | null res     = [Left False]
    | otherwise    = foldr combine [] res
     where
        res = [(Right uni, evals) |
             (cAtom, cAtoms) <- program,
--           trace ("query: "++ (show queryAtomHead) ++ " -> " ++ (show queryAtoms) ++ " rule: " ++ (show clauseAtom) ++ " -> "++ (show clauseAtoms))
             qAtom <?> cAtom,
             let uni = unify qAtom cAtom,
             let evals = eval program ((map (<~ uni) cAtoms) ++ (map (<~ uni) qAtoms)),
             evals /= [Left False]
             ]

        combine :: (Either Bool Substitution, [Either Bool Substitution]) ->
                    [Either Bool Substitution] -> [Either Bool Substitution]
        combine (uni, evals) a = [uni] ++ evals ++ a


--------------------------
--     Test Program     --
--------------------------
program :: Program
program = [(("p", Const "a"), []),
           (("p", Const "b"), []),
           (("p", Const "c"), []),
           (("q", Const "a"), []),
           (("q", Const "b"), []),
           (("r", Var "X"), [("p", Var "X"), ("q", Var "X")])
           ]

query0 = [("r", Const "a")]
query1 = [("r", Const "b")]
query2 = [("r", Const "c")]
query3 = [("r", Var "X")]

test = do
 print (evalOne program query0)
 print (evalOne program query1)
 print (evalOne program query2)
 print (evalOne program query3)
 
 
-- testFile :: String -> Query -> Either Bool [Substitution]
-- 
-- This monad allows arbitrary files and querys to be written in prolog-syntax.
-- 
-- There are a few restictions, 
--  - comments MUST have their own line and MUST start with a '%'.
--  - query's MUST be ended with a '.'.
--
-- It does not properly check syntax, so you could for example end a query with
-- any character you like.

testFile file queryArg = do
    read <- readFile file 
    let program = parseProgram read
        query = parseQuery queryArg
        temp = evalMulti program query
    return temp

parseProgram :: String -> Program
parseProgram string = concat $ map (parseLine) (splitOn "\n" stringPP)
    where stringPP = filter (/= ' ') string 

parseQuery :: String -> [Atom]
parseQuery = (map (fst)).parseAtoms

parseLine :: String -> [Clause]
parseLine [] = []
parseLine ('%':_) = []
parseLine string = [(atom,atoms)]
    where 
        (atom, atomRest) = parseAtom string
        atoms :: [Atom]
        atoms = map fst (parseAtoms (atomRestPP atomRest))
        (atomsStr, _) = break (== '.') $ atomRestPP atomRest
        
        
        atomRestPP (':':'-':x) = x
        atomRestPP ('.':_) = []
        
parseAtoms :: String -> [(Atom, String)]
parseAtoms [] = []
parseAtoms str = (token, str) : (parseAtoms $ trace "a" (tail tokens))
    where 
       (token, tokens) = parseAtom str 

parseAtomR :: (Atom, String) -> (Atom, String)

parseAtomR (_, s) = parseAtom s

parseAtom :: String -> (Atom, String)
parseAtom string = ((mt, map (parseTerm) (splitOn "," ( trace "a" (tail terms)))), tail rest)
    where
         (mt,mRest) = break (\x -> x == '(' || x == '.') string
         (terms, rest) = trace mRest (break (== ')') mRest)

parseTerm :: String -> Term
parseTerm [] = error "empty string"
parseTerm s@(l:ls)  | isUpper l = Variable s
                    | isLower l = Constant s
                    | otherwise = error ((show s) ++ " is not a string")

