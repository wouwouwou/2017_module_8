module LPTypes where

-- Atom:
--   An Atom has a name and terms, which can be constants or variables.
data Atom    = Atom String [Term]
             deriving (Show, Eq)

data Term    = Const String
             | Var String
             deriving (Show, Eq)

-- Clause:
--   Atom is true, if all Atoms are true.
type Clause  = (Atom, [Atom])

-- Program:
--   List of Clauses (definitions).
type Program = [Clause]

-- Query:
--   List of Atoms to be evaluated.
type Query = [Atom]

type Substitution = (Term, Term)

class Expression e where
  (<==) :: e -> Substitution -> e

instance Expression Term where
  -- (<==) (Const a) (Var b, Const c) = null
  -- (<==) (Const a) (Var b, Var c) = null
  (Var a)   <== (Var b, Const c) | a == b = Const c | otherwise = Var a
  (Var a)   <== (Var b, Var c) | a == b = Var c | otherwise = Var a
  (Var _)   <== (Const _, _) = error ""
  (Const a) <== _ = Const a

instance Expression Atom where
  (Atom a []) <== _ = Atom a []
  (Atom a ts) <== s = Atom a (map (<== s) ts)