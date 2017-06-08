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

-- Substitution
--   Tuple of Terms for substitution. The first Term of an expression e will be substituted with
--   the second Term with operator (<==). See also the definition of the operator below.
type Substitution = (Term, Term)

-- Definition of the (<==) operator
--
class Expression e where
  (<==) :: e -> Substitution -> e

instance Expression Term where
  (Var a)   <== (Var b, Const c) | a == b = Const c | otherwise = Var a
  (Var a)   <== (Var b, Var c)   | a == b = Var c   | otherwise = Var a
  (Var _)   <== (Const _, _)              = error ""
  (Const a) <== _                         = Const a

instance Expression Atom where
  (Atom a []) <== _ = Atom a []
  (Atom a ts) <== s = Atom a (map (<== s) ts)

{-

instance Expression Clause where
  (a, as)   <==

  -}
-- TODO: Fix an instance of Expression Clause