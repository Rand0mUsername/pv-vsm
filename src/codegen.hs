{-
    Nikola Jovanovic - Matematicka gimnazija 4D
    Maturski rad iz informatike: 
    Lambda racun i funkcionalno programiranje 
    uz dodatni projekat : izrada kompajlera u programskom jeziku Haskell
    4.2015.
-}

module Codegen where

import Data.Char 
import Data.List
import PV

{-
   Generisanje koda za virtuelnu stek masinu (VSM)
-}

-- generisanje koda prevodi promenljivu tipa Program u String koji sadrzi komande za VSM

codegen :: Program -> String
codegen program = unlines $ (fst $ doStmtBlock program 1) ++ ["HALT"]

-- svaka funkcija prima blok koji treba da bude obradjen i pokazivac na
-- liniju (pocevsi od 1) u zavrsnom kodu od koje treba da pocne, a vraca niz instrukcija
-- dobijen obradom i pokazivac na poslednju liniju u zavrsnom kodu koju ce zauzeti

-- obrada bloka naredbi

doStmtBlock :: StmtBlock -> Int -> ([String], Int) 
doStmtBlock (SingleStmt stmt) ptr = doSingleStmt stmt ptr -- pojedinacna naredba
doStmtBlock (StmtBlock stmt block) ptr = (fstInsts ++ blockInsts, blockPtr) -- blok naredbi
              where (fstInsts, fstPtr) = doSingleStmt stmt ptr
                    (blockInsts, blockPtr) = doStmtBlock block (fstPtr + 1)

-- obrada pojedinacne naredbe

doSingleStmt :: Stmt -> Int -> ([String], Int)
doSingleStmt (Skip) ptr = (["NOP"], ptr) -- nije neophodno, ali citljivije
doSingleStmt (Ass name aExp1) ptr = (aExpInsts ++ ["STORE " ++ name], aExpPtr + 1)
               where (aExpInsts, aExpPtr) = doAExp1 aExp1 ptr -- operacija dodele

              -- assemblerski mehanizam za simulaciju if/else naredbi
doSingleStmt (IfElse bExp1 ifBlock elseBlock) ptr = (allInsts, elsePtr) 
	           where (bExpInsts, bExpPtr) = doBExp1 bExp1 ptr
	                 (ifInsts, ifPtr) = doStmtBlock ifBlock (bExpPtr + 2)
	                 (elseInsts, elsePtr) = doStmtBlock elseBlock (ifPtr + 2)
	                 skipIf = ["JZ " ++ show (ifPtr + 2)]
	                 skipElse = ["JMP " ++ show (elsePtr + 1)]
	                 allInsts = bExpInsts ++ skipIf ++ ifInsts ++ skipElse ++ elseInsts

              -- assemblerski mehanizam za simulaciju while ciklusa
doSingleStmt (WhileDo bExp1 doBlock) ptr = (allInsts, doPtr + 1)
	           where (bExpInsts, bExpPtr) = doBExp1 bExp1 ptr
	                 (doInsts, doPtr) = doStmtBlock doBlock (bExpPtr + 2)
	                 skipDo = ["JZ " ++ show (doPtr + 2)]
	                 newIter = ["JMP " ++ show ptr]
	                 allInsts = bExpInsts ++ skipDo ++ doInsts ++ newIter

-- obrada aritmetickih izraza

doAExp1 :: Aexp1 -> Int -> ([String], Int)
doAExp1 (Aexp1' aExp2) ptr = doAExp2 aExp2 ptr
doAExp1 (Add aExp2 aExp1) ptr = (aExp2Insts ++ aExp1Insts ++ ["ADD"], aExp1Ptr + 1)
	      where (aExp2Insts, aExp2Ptr) = doAExp2 aExp2 ptr
	            (aExp1Insts, aExp1Ptr) = doAExp1 aExp1 (aExp2Ptr + 1) 

doAExp2 :: Aexp2 -> Int -> ([String], Int)
doAExp2 (Aexp2' aExp3) ptr = doAExp3 aExp3 ptr
doAExp2 (Mul aExp3 aExp2) ptr = (aExp3Insts ++ aExp2Insts ++ ["MUL"], aExp2Ptr + 1)
	      where (aExp3Insts, aExp3Ptr) = doAExp3 aExp3 ptr
	            (aExp2Insts, aExp2Ptr) = doAExp2 aExp2 (aExp3Ptr + 1) 

doAExp3 :: Aexp3 -> Int -> ([String], Int)
doAExp3 (ParensA aExp1) ptr = doAExp1 aExp1 ptr
doAExp3 (Id name) ptr = (["LOAD " ++ name], ptr)
doAExp3 (Number val) ptr = (["PUSH " ++ show val], ptr)

-- obrada logickih izraza

doBExp1 :: Bexp1 -> Int -> ([String], Int)
doBExp1 (Bexp1' bExp2) ptr = doBExp2 bExp2 ptr
doBExp1 (Or bExp2 bExp1) ptr = (bExp2Insts ++ bExp1Insts ++ ["OR"], bExp1Ptr + 1)
	      where (bExp2Insts, bExp2Ptr) = doBExp2 bExp2 ptr
	            (bExp1Insts, bExp1Ptr) = doBExp1 bExp1 (bExp2Ptr + 1) 

doBExp2 :: Bexp2 -> Int -> ([String], Int)
doBExp2 (Bexp2' bExp3) ptr = doBExp3 bExp3 ptr
doBExp2 (And bExp3 bExp2) ptr = (bExp3Insts ++ bExp2Insts ++ ["AND"], bExp2Ptr + 1)
	      where (bExp3Insts, bExp3Ptr) = doBExp3 bExp3 ptr
	            (bExp2Insts, bExp2Ptr) = doBExp2 bExp2 (bExp3Ptr + 1) 

doBExp3 :: Bexp3 -> Int -> ([String], Int)
doBExp3 (ParensB bExp1) ptr = doBExp1 bExp1 ptr
doBExp3 (Boolean True) ptr = (["PUSH 1"], ptr)
doBExp3 (Boolean False) ptr = (["PUSH 0"], ptr)
doBExp3 (Negation bExp1) ptr = (bExp1Insts ++ ["NOT"], bExp1Ptr + 1)
           where (bExp1Insts, bExp1Ptr) = doBExp1 bExp1 ptr

           -- relacije se resavaju koriscenjem uslovnih skokova
doBExp3 (Relation left sgn right) ptr = (allInsts, (rightPtr + 5))
	      where (leftInsts, leftPtr) = doAExp1 left ptr
	            (rightInsts, rightPtr) = doAExp1 right (leftPtr + 1)
	            trueJump = ["SUB", getTrueJump sgn ++ show (rightPtr + 5)]
	            pushBlock = ["PUSH 0", "JMP " ++ show (rightPtr + 6), "PUSH 1" ] 
	            allInsts = leftInsts ++ rightInsts ++ trueJump ++ pushBlock

-- svaka relacija koristi jedan uslovni skok
getTrueJump :: Ordering -> String
getTrueJump GT = "JP "
getTrueJump LT = "JM "
getTrueJump EQ = "JZ "