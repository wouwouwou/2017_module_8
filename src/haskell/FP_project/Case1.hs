module Case1 where

import LPTypes
import LPEvaluator

program :: Program
program = [(Atom "a0" [], []),
           (Atom "a1" [], []),
           (Atom "a2" [], []),
           (Atom "b0" [], [Atom "a0" [], Atom "a1" []]),
           (Atom "b1" [], [Atom "a1" [], Atom "a2" []]),
           (Atom "b2" [], [Atom "a0" [], Atom "a1" [], Atom "d" []]),
           (Atom "c0" [], [Atom "b0" [], Atom "b1" []]),
           (Atom "c1" [], [Atom "b0" [], Atom "b1" [], Atom "b2" []])
           ]

query0 = [Atom "c0" []]
query1 = [Atom "c1" []]

test = do
  print (evalProp program query0)
  print (evalProp program query1)