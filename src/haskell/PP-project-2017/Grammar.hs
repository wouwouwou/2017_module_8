{-

    Declarations have a type.
    
    Statements are ended with a semicolon.
    
 -- Standard language features --
    
    We support procedures:
    
    procedure Pid ([TYPE PVAR1, ... , TYPE PVARN]) STAT
    
    We support the following statements:
    
    if (EXPR) STAT [else STAT] ;
    while (EXPR) STAT ;
    TYPE VAR [= EXPR] ;
    Pid ([PVAR1, ... , PVARN]) ;        -- if PVAR is a naked variable, it is passed call-by-reference
    
    We use blocks to define groups of statements. These are also used in scoping.
    
    We support the following expressions:
    
    VAR = EXPR -> EXPR
    BOOL < ==,!=,&&,||,<> > BOOL -> BOOL
    ! BOOL -> BOOL
    INT < +,-,*,/,^,% > INT -> INT
    INT < <,>,<=,>=,==,!= > INT -> BOOL
    
    
 -- Concurrent features --
    
    We support the following statements:
    
    global TYPE VAR [= (EXPR)] ;
    
    fork Pid ([PVAR1, ... , PVARN]) ;   -- if PVAR is a naked variable, it is passed call-by-reference
    join ;
    



-}

{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}
-- Necessary for function toRoseTree

module Grammar where

{- ===========================================================================
Contains the grammar + examples of test definitions
NOTE: Compiler directives above
=========================================================================== -}

import FPPrac.Trees         -- Contains the function toRoseTree.
import GHC.Generics         -- Necessary for correct function of FPPrac.

import Types                -- Extend the file TypesEtc with your own alphabet
import FP_ParserGen (parse) -- Touching this file leaves you at your own devices

-- ==========================================================================================================
-- Example grammar, to illustrate the structure of the definition of the grammar as a function
--      (where the Alphabet is in the file TypesEtc.hs)

grammar :: Grammar
grammar nt = case nt of

    -- Program
    Program ->  [[ (*:) [Global], (*:) [Enum], (*:) [Proc], (*:) [Stat] ]]

    -- Globals
    Global  ->  [[ global, Type, Var, (?:) [ass, Expr], eol ]]

    -- Enumerations
    Enum    ->  [[ Type, Var, ass, lBrace, Var, (*:) [comma, Var], rBrace, eol ]]

    -- Procedures
    Proc    ->  [[ procedure, Pid, lPar, (?:) [Type, Var, (*:) [comma, Type, Var]], rPar, Stat ]]

    -- Statements
    Stat    ->  [[ Type, Var, (?:) [ass, Expr], eol ]                               -- declaration
                ,[ ifStr, lPar, Expr, rPar, Stat, (?:) [elseStr, Stat] ]            -- if
                ,[ while, lPar, Expr, rPar, Stat ]                                  -- while
                ,[ fork, Pid, lPar, (?:) [Expr, (*:) [comma, Expr]], rPar, eol ]    -- fork
                ,[ join, eol ]                                                      -- join
                ,[ Pid, lPar, (?:) [Expr, (*:) [comma, Expr]], rPar, eol ]          -- call
                ,[ Expr, eol ]                                                      -- expression
                ,[ lBrace, (*:) [Stat], rBrace ]                                    -- block
                ,[ printStr, lPar, Expr, (*:) [comma, Expr], rPar, eol ]            -- print
                ,[ Var, ass, Expr, eol ]]                                           -- assign

    -- Expressions. Already implements the fact that some operations are bound tighter than
    -- other operations (operator precedence). The order from tightest to loosest: Mul, PlusMin,
    -- Ord, Equal, And, XOR, Or.
    Expr      ->  [[ OR, (?:) [Expr'] ]]                   -- Expr -> OR (?:) Expr'

    Expr'     ->  [[ opOr, OR, (?:) [Expr'] ]]             -- opOr, OR (?:) Expr'

    OR        ->  [[ XOr, (?:) [OR'] ]]                    -- XOr, (?:) Or'

    OR'       ->  [[ opXor, XOr, (?:) [OR'] ]]             -- opXor, XOr, (?:) OR'

    XOr       ->  [[ AND, (?:) [XOr'] ]]                   -- AND, (?:) XOr'

    XOr'      ->  [[ opAnd, AND, (?:) [XOr'] ]]            -- opAnd, AND, (?:) XOr'

    AND       ->  [[ EQUAL, (?:) [AND'] ]]                 -- EQUAL, (?:) AND'

    AND'      ->  [[ opEqual, EQUAL, (?:) [AND'] ]]        -- opEqual, EQUAL, (?:) AND'

    EQUAL     ->  [[ Ord, (?:) [EQUAL'] ]]                 -- Ord, (?:) EQUAL'

    EQUAL'    ->  [[ opOrd, Ord, (?:) [EQUAL'] ]]          -- opOrd, Ord, (?:) EQUAL'

    Ord       ->  [[ Term, (?:) [Ord'] ]]                  -- Term, (?:) Ord'

    Ord'      ->  [[ opPlusMin, Term, (?:) [Ord'] ]]       -- opPlusMin, Term, (?:) Ord'

    Term      ->  [[ Factor, (?:) [Term'] ]]               -- Factor (?:) Term'

    Term'     ->  [[ opMul, Factor, (?:) [Term'] ]]        -- OpMulDiv Factor (?:) Term'

    Factor    ->  [[ lPar, Expr, rPar]                     -- (Expr)
                  ,[ PreUnary, Expr ]                      -- Prefix
                  ,[ Var ]                                 -- Var
                  ,[ IntType ]                             -- Int
                  ,[ BoolType ]]                           -- Bool

    -- Other
    PreUnary  ->  [[ opPlusMin ]                           -- Prefix unary + and -
                  ,[ opIncDec ]                            -- Prefix unary ++ and --
                  ,[ opNot ]]                              -- Prefix unary !

    Type      ->  [[ typeStr ]]                            -- type

    Var       ->  [[ var ]]                                -- variable

    Pid       ->  [[ Var ]]                                -- procedure identifier

    IntType   ->  [[ intType ]]                            -- number

    BoolType  ->  [[ boolType ]]                           -- boolean


-- shorthand names can be handy, such as:
lPar        = Symbol "("            -- Terminals WILL be shown in the parse tree
rPar        = Symbol ")"            -- Symbols WILL NOT be shown in the parse tree
lBrace      = Terminal "{"
rBrace      = Symbol "}"
procedure   = Symbol "procedure"
ifStr       = Terminal "if"
elseStr     = Terminal "else"
while       = Terminal "while"
ass         = Terminal "="
fork        = Terminal "fork"
join        = Terminal "join"
global      = Symbol "global"
printStr    = Terminal "print"


eol         = Symbol ";"
comma       = Symbol ","

var         = SyntCat Var
intType     = SyntCat IntType
boolType    = SyntCat BoolType
typeStr     = SyntCat Type

opMul       = SyntCat OpMul
opPlusMin   = SyntCat OpPlusMin
opIncDec    = SyntCat OpIncDec
opOrd       = SyntCat OpOrd
opEqual     = SyntCat OpEqual
opNot       = SyntCat OpNot
opAnd       = SyntCat OpAnd
opOr        = SyntCat OpOr
opXor       = SyntCat OpXor



-- ==========================================================================================================
-- TESTING: example expression: "((10+20)*30)"

-- Result of tokenizer (to write yourself) should be something like:
{-tokenList0 = [ (Bracket,"(",0)
             , (Bracket,"(",1)
             , (Nmbr,"10",2)
             , (Op,"+",3)
             , (Nmbr,"20",4)
             , (Bracket,")",5)
             , (Op,"*",6)
             , (Nmbr,"30",7)
             , (Bracket,")",8)
             ]-}

-- Parse this tokenlist with a call to the function parse, with
--      - grammar: the name of the grammar above
--      - Expr: the start-nonterminal of the grammar above
--      - tokenList0: the tokenlist above
--parseTree0 = parse grammar Expr tokenList0

-- prpr: for pretty-printing the parsetree, including error messages
--testTxt    = prpr parseTree0

-- showTree + toRoseTree: for graphical representation in browser
--testGr     = showTree $ toRoseTree parseTree0
