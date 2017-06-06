module LPEvaluator where

import LPTypes
import Data.Maybe
import Data.Either
import Debug.Trace

------------Level 1------------
evalProp :: Program -> Query -> Bool
evalProp [] _           = False
evalProp _ []           = True
evalProp program (q:qs) | res == [] = False
                        | otherwise = True
    where
        res = [ True | (c, cs) <- program, {- trace traceLine True, -} c == q, evalProp program (cs ++ qs) ]

        {- traceLine = ("query: "++ (show q) ++ " ++ " ++ (show qs) ++ " rule: " ++ (show c) ++ " -> "++ (show cs)) -}


------------Level 2------------
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

       vars = [x | let y = (getTerms query), x@(Var _) <- y]

       getTerms :: Query -> [Term]
       getTerms [] = []
       getTerms [(Atom _ [t])] = [t]
       getTerms ((Atom _ [t]):as) = [t] ++ getTerms as


eval :: Program -> Query -> [Either Bool Substitution]
eval [] _ = [Left False]
eval _ [] = [Left True]
eval program query@(queryHead:queryAtoms)
    | null res     = [Left False]
    | otherwise    = foldr (\(f, s) a -> [f] ++ (s) ++ a) [] res
    where
        res = [(Right uni, evals) |
               clause@(clauseAtom@(Atom _ _), clauseAtoms) <- program,
               queryHead <?> clauseAtom,
               let uni = unify queryHead clauseAtom,
               let evals = eval program ((map (<== uni) clauseAtoms) ++ (map (<== uni) queryAtoms)),
               evals /= [Left False]
               ]

--Rename
------------------------------------------------------------------------
rename :: Program -> Query -> Program
rename program [] = program
rename program query = init $ foldl renamePerAtom
                              (program ++ [((Atom "_query" [(Const "a")]), query)])
                              query

renamePerAtom :: Program -> Atom -> Program
renamePerAtom program (Atom _ (oldterm@(Var _):ts)) =
  map (renameClause (oldterm, (Var newvar))) program
   where
     newvar = generateVar varlist program

     renameClause :: Substitution -> Clause -> Clause
     renameClause sub (a, as) = (a <== sub, map (<== sub) as)

renamePerAtom program _ = program


generateVar :: [String] -> Program -> String
generateVar [] program               = error "No free variable found!"
generateVar (newvar:seed) program    | elem newvar (map getstr $ concat vars)
                                          = generateVar varlist program
                                     | otherwise
                                          = newvar
      where
          getstr (Var str)      = str
          getstr (Const str)    = str
          vars                  = concat $ map clausevars program
          clausevars            = (\((Atom _ var), ats) -> var : (map atomvars ats))
          atomvars              = (\(Atom _ var) -> var)

varlist :: [String]
varlist = [nextchar ++ [alph] | nextchar <- "" : varlist, alph <- ['A'..'Z']]


--Unify
------------------------------------------------------------------------

unify :: Atom -> Atom -> Substitution

unify (Atom firstpred (firstconst@(Const _):firstterms))
      (Atom secondpred (secondconst@(Const _):secondterms))
                   | firstpred /= secondpred     = error "Can not unify: predicates not equal!"
                   | firstconst == secondconst   = (firstconst, secondconst)
                   | otherwise                   = error "Can not unify: constants not equal!"

unify (Atom firstpred (var@(Var _):firstterms))
      (Atom secondpred (const@(Const _):secondterms))
                   | firstpred == secondpred     = (var, const)
                   | otherwise                   = error "Can not unify: predicates not equal!"

unify (Atom firstpred (const@(Const _):firstterms))
      (Atom secondpred (var@(Var _):secondterms))
                   | firstpred == secondpred     = (var, const)
                   | otherwise                   = error "Can not unify: predicates not equal!"

unify (Atom firstpred (firstvar@(Var _):firstterms))
      (Atom secondpred (secondvar@(Var _):secondterms))
                   | firstpred == secondpred     = (secondvar, firstvar)
                   | otherwise                   = error "Can not unify: predicates not equal!"


(<?>) :: Atom -> Atom -> Bool
(Atom firstpred (firstconst@(Const _):firstterms)) <?> (Atom secondpred (secondconst@(Const _):secondterms))
           | firstpred /= secondpred     = False
           | firstconst == secondconst   = True
           | otherwise                   = False

(Atom firstpred (var@(Var _):firstterms)) <?> (Atom secondpred (const@(Const _):secondterms))
           | firstpred == secondpred     = True
           | otherwise                   = False

(Atom firstpred (const@(Const _):firstterms)) <?> (Atom secondpred (var@(Var _):secondterms))
           | firstpred == secondpred     = True
           | otherwise                   = False

(Atom firstpred (firstvar@(Var _):firstterms)) <?> (Atom secondpred (secondvar@(Var _):secondterms))
           | firstpred == secondpred     = True
           | otherwise                   = False