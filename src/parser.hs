{-
    Nikola Jovanovic - Matematicka gimnazija 4D
    Maturski rad iz informatike: 
    Lambda racun i funkcionalno programiranje 
    uz dodatni projekat : izrada kompajlera u programskom jeziku Haskell
    4.2015.
-}

module Parser where

import Data.Char 
import Data.List
import Data.Maybe
import PV

{-
   Sintaksna analiza programa (parsiranje)
-}

-- parsiranje prevodi niz tokena u promenljivu tipa Program
-- program mora biti blok naredbi + EOF

parse :: [Token] -> Program
parse [] = error "Parser error"
parse ts
        | (head rest /= EOF) = error "Parser error" 
        | otherwise = program
        where (program, rest) = parseStmtBlock ts

-- svaka funkcija za parsiranje ce vracati rezultat parsiranja i preostali niz nepariranih tokena

-- parsiranje bloka naredbi

parseStmtBlock :: [Token] -> (StmtBlock, [Token])
parseStmtBlock ts
             | (head rest /= SEMICOLON) = (SingleStmt stmt, rest) 
             | otherwise = let (block, blockRest) = parseStmtBlock . tail $ rest 
                           in (StmtBlock stmt block, blockRest) 
             where (stmt, rest) = parseStmt ts

-- parsiranje pojedinacne naredbe

parseStmt :: [Token] -> (Stmt, [Token])
parseStmt (KWRD_SKIP : ts) = (Skip, ts)
parseStmt (KWRD_IF : ts) = (IfElse b1 stmtThen stmtElse, rest)
	                   where (b1, restB) = parseB1 ts
	                         (stmtThen, restThen) = parseStmtBlock (drop 2 restB)
	                         (stmtElse, restElse) = parseStmtBlock (drop 3 restThen)
	                         rest = tail restElse 
parseStmt (KWRD_WHILE : ts) = (WhileDo b1 stmtDo, rest)
	                      where (b1, restB) = parseB1 ts
	                            (stmtDo, restDo) = parseStmtBlock (drop 2 restB)
	                            rest = tail restDo 
parseStmt (ID name : OP_ASS : ts) = (Ass name a1, rest)
                                where (a1, rest) = parseA1 ts                               
parseStmt ts = error "Parser error"

-- parsiranje aritmetickih izraza

parseA1 :: [Token] -> (Aexp1, [Token])
parseA1 ts 
      | (head a2rest == OP_PLUS) = let (a1, rest) = parseA1 . tail $ a2rest 
                                   in (Add a2 a1, rest) 
      | otherwise = (Aexp1' a2, a2rest)
      where (a2, a2rest) = parseA2 ts

parseA2 :: [Token] -> (Aexp2, [Token])
parseA2 ts 
      | (head a3rest == OP_MUL) = let (a2, rest) = parseA2 . tail $ a3rest 
                                   in (Mul a3 a2, rest) 
      | otherwise = (Aexp2' a3, a3rest)
      where (a3, a3rest) = parseA3 ts

-- terminali za aritmeticke izraze su brojevi (NUM) i imena promenljivih (ID)

parseA3 :: [Token] -> (Aexp3, [Token])
parseA3 (PAREN_LEFT : ts) = let (a1, rest) = parseA1 ts 
                          in if (head rest == PAREN_RIGHT) then (ParensA a1, tail rest) 
                          	                               else error "Parser error"
parseA3 (NUM n : ts) = (Number n, ts)
parseA3 (ID s : ts) = (Id s, ts) 
parseA3 ts = error "Parser error"

-- parsiranje logickih izraza

parseB1 :: [Token] -> (Bexp1, [Token])
parseB1 ts 
      | (head b2rest == KWRD_OR) = let (b1, rest) = parseB1 . tail $ b2rest 
                                   in (Or b2 b1, rest) 
      | otherwise = (Bexp1' b2, b2rest)
      where (b2, b2rest) = parseB2 ts

parseB2 :: [Token] -> (Bexp2, [Token])
parseB2 ts 
      | (head b3rest == KWRD_AND) = let (b2, rest) = parseB2 . tail $ b3rest 
                                   in (And b3 b2, rest) 
      | otherwise = (Bexp2' b3, b3rest)
      where (b3, b3rest) = parseB3 ts

-- terminali za logicke izraze su logicke vrednosti (BOOL) i relacije medju a. izrazima (A1)

parseB3 :: [Token] -> (Bexp3, [Token])
parseB3 (PAREN_LEFT : ts) = let (b1, rest) = parseB1 ts 
                          in if (head rest == PAREN_RIGHT) then (ParensB b1, tail rest) 
                          	                               else error "paren failed"
parseB3 (BOOL b : ts) = (Boolean b, ts)
parseB3 (OP_NOT : ts) = let (b1, rest) = parseB1 ts in (Negation b1, rest) 
parseB3 ts 
      | isJust ordSign = (Relation left (fromJust ordSign) right, rightRest)
      | otherwise = error []
	  where (left, leftRest) = parseA1 ts
	        ordSign = getOrdering . head $ leftRest
	        (right, rightRest) = parseA1 . tail $ leftRest

-- pomocna funkcija koja izvlaci Ordering iz tokena OP_ORD

getOrdering :: Token -> Maybe Ordering  
getOrdering (OP_ORD ordSign) = Just ordSign   
getOrdering t = Nothing