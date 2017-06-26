By Martijn Verkleij (s1466895) & Wouter Bos (s1606824)

Case 1:
Case1.hs
Usage: evalProp Program Query
Alternative usage: test
One programs is provided in the form of "program".
Two queries are provided in the form of "query0" and "query1".
The following types are relevant for using this program:
	data Term       = Atom String
	type Clause     = (Term, [Term])
	type Program    = [Clause]
	type Query      = [Term]
Examples:
	test
	evalProp program query1

Case 2:
Case2.hs
Usage: evalOne Program Query
Alternative usage: test
A program is provided in the form of "program".
Four queries are provided in the form of "query0", "query1", "query2" and "query3".
The following types are relevant for the usage:
	data Term           = Const String | Var String
	type Atom           = (String, Term)
	type Clause         = (Atom, [Atom])
	type Program        = [Clause]
	type Query          = [Atom]
	type Substitution   = (Term, Term)
Example:
    test
	evalOne program query0
	evalOne program [("r", Const "a"), ("r", Var "X")]

Case 3:
Case3.hs
Usage: evalMulti Program Query
Alternative usage: test
One program is provided in the form of "program".
Four queries are provided in the form of "query1", "query2", "query3" and "query4".
One (very sensitive) prolog file interpreter is provided in the form of "testFile".
testFile usage: testFile Filepath PrologQuery
The following types are relevant for the usage:
	data Term           = Const String | Var String
	type Atom           = (String, [Term])
	type Clause         = (Atom, [Atom])
	type Program        = [Clause]
	type Query          = [Atom]
	type Substitution   = (Term, Term)
Examples:
    test
    evalMulti program query1
	evalMulti program [("s", [Var "Z"]), ("q", [Var "Y", Var "Z"])]
	testFile "filepath.pl" "s(Z)."