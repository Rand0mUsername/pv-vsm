{-
    Nikola Jovanovic - Matematicka gimnazija 4D
    Maturski rad iz informatike: 
    Lambda racun i funkcionalno programiranje 
    uz dodatni projekat : izrada kompajlera u programskom jeziku Haskell
    4.2015.
-}

module PV where

{-
   Predstavljanje podataka u vezi sa jezikom PV
   neophodno za dalji proces kompilacije
-}

-- alias za stringove koriscene kao ID

type Id_String = String

-- alias za blok naredbi
-- jedan PV program ce biti sadrzan u promenljivoj tipa Program

type Program = StmtBlock 

-- blok naredbi, pojedinacne naredbe su odvojene ';'
-- nakon poslednje naredbe u bloku ne stoji ';'

data StmtBlock = StmtBlock Stmt StmtBlock
               | SingleStmt Stmt
               deriving (Show)

-- pojedinacna naredba

data Stmt = IfElse Bexp1 StmtBlock StmtBlock
          | WhileDo Bexp1 StmtBlock
          | Skip 
          | Ass Id_String Aexp1
           deriving (Show)

-- Aritmeticki izrazi

data Aexp1 = Add Aexp2 Aexp1 
           | Aexp1' Aexp2
           deriving (Show)

data Aexp2 = Mul Aexp3 Aexp2 
           | Aexp2' Aexp3
           deriving (Show)

data Aexp3 = ParensA Aexp1 
           | Id Id_String
           | Number Int
           deriving (Show)

-- Logicki izrazi

data Bexp1 = Or Bexp2 Bexp1
           | Bexp1' Bexp2
           deriving (Show)

data Bexp2 = And Bexp3 Bexp2
           | Bexp2' Bexp3
           deriving (Show)

data Bexp3 = Boolean Bool 
           | ParensB Bexp1 
           | Negation Bexp1
           | Relation Aexp1 Ordering Aexp1
           deriving (Show)

-- Tokeni u koje ce pretvarati lekser

data Token = NUM Int -- 1, 2, -3, ...
           | ID Id_String -- a, b, cC1, ...
           | BOOL Bool -- True/False
           | SEMICOLON  -- ;
           | PAREN_LEFT -- (
           | PAREN_RIGHT -- )
           | BRKT_LEFT -- {
           | BRKT_RIGHT -- }
           | KWRD_AND -- and
           | KWRD_OR -- or
           | KWRD_IF -- if
           | KWRD_THEN -- then
           | KWRD_ELSE -- else
           | KWRD_WHILE -- while
           | KWRD_DO -- do
           | KWRD_SKIP -- skip
           | OP_NOT -- !
           | OP_ASS -- :=
           | OP_PLUS -- +
           | OP_MUL -- *
           | OP_ORD Ordering -- >, <, =
           | EOF -- zavrsni token
           deriving (Eq, Show)