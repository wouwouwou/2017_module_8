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
getSubstitutions :: Program -> Query -> [Substitution]
getSubstitutions program query = zip (nub $ intersect programvars queryvars) (map (\x -> Var x) newvars)
    where
         programvars = [x | let z = (map (fst) program) ++ (concat $ map (snd) program),
                            let y = concat $ map (snd) z,
                            x@(Var _) <- y ]
         queryvars   = [x | let y = concat $ map (snd) query,
                            x@(Var _) <- y ]
         newvars     = generateVars varlist (program ++ [(("_query", [Const "a"]), query)])


-- -- Generatevars -- --
-- -- Returns a list of variables which can be used as unique new  -- --
-- -- variables in a Program. Parameters:                          -- --
-- -- (Possible Variable Names, Program)                           -- --
-- -- Example: newvars = generateVars varlist program              -- --
------------------------------------------------------------------------
generateVars :: [String] -> Program -> [String]
generateVars [] program               = error "No free variable found!"
generateVars (newvar:seed) program    | elem newvar (map getstr $ concat vars)
                                          = generateVars seed program
                                      | otherwise
                                          = newvar : generateVars seed program
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
-- -- Returns a substitution if the two atoms are unifiable.       -- --
-- -- Otherwise returns the boolean False.                         -- --
------------------------------------------------------------------------
unify :: Atom -> Atom -> Either Bool [Substitution]
unify (_, []) _     = Left False
unify _ (_, [])     = Left False
unify atom1 atom2
          | length (snd atom1) /= length (snd atom2) = Left False
          | otherwise                                = unify' atom1 atom2
          where
             unify' :: Atom -> Atom -> Either Bool [Substitution]
             unify' (_, []) (_, []) = Right []
             unify' (firstpred,  firstterm@(Const _):firstterms)
                    (secondpred, secondterm@(Const _):secondterms)
                        | firstpred  /= secondpred   = Left False
                        | firstterm == secondterm    = Right (uni' ++ (concat $ rights $ [unify' atom1 atom2]))
                        | otherwise                  = Left False
                        where
                          uni                        = (firstterm, secondterm)
                          atom1                      = (firstpred, firstterms)  <~ uni
                          atom2                      = (firstpred, secondterms) <~ uni
                          uni'                       = [uni]

             unify' (firstpred,  firstterm@(Var _):firstterms)
                    (secondpred, secondterm@(Const _):secondterms)
                        | firstpred == secondpred    = Right (uni' ++ (concat $ rights $ [unify' atom1 atom2]))
                        | otherwise                  = Left False
                        where
                         uni                        = (firstterm, secondterm)
                         atom1                      = (firstpred, firstterms)  <~ uni
                         atom2                      = (firstpred, secondterms) <~ uni
                         uni'                       = [uni]

             unify' (firstpred,  firstterm@(Const _):firstterms)
                    (secondpred, secondterm@(Var _):secondterms)
                        | firstpred == secondpred    = Right (uni' ++ (concat $ rights $ [unify' atom1 atom2]))
                        | otherwise                  = Left False
                        where
                          uni                        = (firstterm, secondterm)
                          atom1                      = (firstpred, firstterms)  <~ uni
                          atom2                      = (firstpred, secondterms) <~ uni
                          uni'                       = [uni]

             unify' (firstpred,  firstterm@(Var _):firstterms)
                    (secondpred, secondterm@(Var _):secondterms)
                        | firstpred == secondpred    = Right (uni' ++ (concat $ rights $ [unify' atom1 atom2]))
                        | otherwise                  = Left False
                        where
                          uni                        = (firstterm, secondterm)
                          atom1                      = (firstpred, firstterms)  <~ uni
                          atom2                      = (firstpred, secondterms) <~ uni
                          uni'                       = [uni]


-- -- evalMulti -- --
-- -- Evaluates a query for a program after renaming. After        -- --
-- -- evaluating, filters and trims the result is either a list    -- --
-- -- with a single boolean, or a list of substitutions for which  -- --
-- -- the query becomes true.                                      -- --
------------------------------------------------------------------------
evalMulti :: Program -> Query -> [Either Bool [Substitution]]
evalMulti [] _           = error "The program is empty!"
evalMulti _ []           = error "The query is empty!"
evalMulti program query  | null $ rightRes    = filter (isLeft) res
                         | otherwise          = rightRes
   where
       rightRes = filter (isRight) noConstants
       noConstants = trim res
       res = eval (rename program query) query

       trim :: [Either Bool [Substitution]] -> [Either Bool [Substitution]]
       trim [] = []
       trim ((Right x):xs)
           | null $ trim' x    = trim xs
           | otherwise         = Right (trim' x) : trim xs
       trim (x:xs)             = trim xs

       trim' :: [Substitution] -> [Substitution]
       trim' [] = []
       trim' (x@(term1@(Var _), term2@(Var _)):xs)
           | elem term1 vars && elem term2 vars    = x : (trim' xs)
           | otherwise                             = trim' xs
       trim' (x@(term1@(Var _), term2@(Const _)):xs)
           | elem term1 vars                       = x : (trim' xs)
           | otherwise                             = trim' xs
       trim' (_:xs)                                = trim' xs

       vars = [x | let y = concat $ map (snd) query, x@(Var _) <- y]


-- -- eval -- --
-- -- The evaluating part of evalMulti. Looks if the queried atom is -- --
-- -- unifiable. If so, adds the corresponding substitution to a   -- --
-- -- list. After that, it a recursive call will be done for the   -- --
-- -- remaining clause- and query-atoms. In the end, this          -- --
-- -- function will return a list with booleans and substitutions. -- --
------------------------------------------------------------------------
eval :: Program -> Query -> [Either Bool [Substitution]]
eval [] _ = [Left False]
eval _ [] = [Left True]
eval program (qAtom:qAtoms)
    | null res     = [Left False]
    | otherwise    = foldr combine [] res
     where
        res = [(Right uni, evals) |
             (cAtom, cAtoms) <- program,
--           trace ("query: "++ (show queryAtomHead) ++ " -> " ++ (show queryAtoms) ++ " rule: " ++ (show clauseAtom) ++ " -> "++ (show clauseAtoms))
             let unification = unify qAtom cAtom,
             isRight unification,
             let (Right uni) = unification,
             let evals = eval program ((foldl (<~) cAtoms uni) ++ (foldl (<~) qAtoms uni)),
             evals /= [Left False]
             ]

        combine :: (Either Bool [Substitution], [Either Bool [Substitution]]) ->
                    [Either Bool [Substitution]] -> [Either Bool [Substitution]]
        combine (uni, evals) a = [uni] ++ evals ++ a
 
 
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

testFile :: FilePath -> String -> IO [Either Bool [Substitution]]
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
parseTerm s@(l:ls)  | isUpper l = Var s
                    | isLower l = Const s
                    | otherwise = error ((show s) ++ " is not a string")

