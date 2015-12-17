{-
    Nikola Jovanovic - Matematicka gimnazija 4D
    Maturski rad iz informatike: 
    Lambda racun i funkcionalno programiranje 
    uz dodatni projekat : izrada kompajlera u programskom jeziku Haskell
    4.2015.
-}

module Lexer where

import Data.Char 
import Data.List
import PV

{-
   Leksicka analiza programa
-}

-- leksicka analiza prevodi String program u niz Tokena

lexx :: String -> [Token]
lexx program 
      | (clean == []) = [EOF]
      | otherwise = token : (lexx rest)
      where (token, rest) = nextToken clean
            clean = stripComments . stripSeparators $ program

-- uklanjanje komentara i separatora 

stripComments :: String -> String
stripComments [] = []
stripComments ( '/' : xs) = stripComments . stripSeparators . tail $ dropWhile (/= '/') xs
stripComments s = s

stripSeparators :: String -> String
stripSeparators [] = []
stripSeparators s = dropWhile isSep s

-- detektovanje separatora

isSep :: Char -> Bool 
isSep '\r' = True
isSep '\n' = True
isSep '\t' = True
isSep ' ' = True
isSep c = False

-- vracanje sledeceg tokena i neprocesiranog dela programa
-- String s nikad nije prazan, sledi iz funkcije lexx

nextToken :: String -> (Token, String)
nextToken s @ (x:xs) 
        | isAlpha x = readKwrdIdBool s -- 1. pocinju slovom
        | isDigit x = readNum s '+'    -- 2. pocinju brojem
        | (x == ':') = readAss s       -- 3. operacija dodele (2 znaka)
        | (x == '-') = readNum s '-'   -- 4. negativni brojevi
        | otherwise = (readSign x, xs) -- 5. svi preostali tokeni (1 znak)

-- obrada tokena:
-- KWRD_AND KWRD_OR KWRD_IF KWRD_THEN KWRD_ELSE KWRD_WHILE KWRD_DO KWRD_SKIP ID BOOL

readKwrdIdBool :: String -> (Token, String)
readKwrdIdBool s 
        | (word == "and") = (KWRD_AND, rest)
        | (word == "or") = (KWRD_OR, rest)
        | (word == "if") = (KWRD_IF, rest)
        | (word == "then") = (KWRD_THEN, rest)
        | (word == "else") = (KWRD_ELSE, rest)
        | (word == "while") = (KWRD_WHILE, rest)
        | (word == "do") = (KWRD_DO, rest)
        | (word == "skip") = (KWRD_SKIP, rest)
        | (word == "true") = (BOOL True, rest)
        | (word == "false") = (BOOL False, rest)
        | otherwise = (ID idWord, idRest)
        where (word, rest) = span isAlpha s
              (idWord, idRest) = span isAlphaNum s

-- obrada tokena NUM (pozitivni i negativni brojevi)

readNum  :: String -> Char -> (Token, String)
readNum s '+' = (NUM (read number), rest)
    where (number, rest) = span isDigit s
readNum (x:xs) '-' = (NUM (negate $ read number), rest)
    where (number, rest) = span isDigit xs

-- obrada tokena OP_ASS, operacija dodele

readAss :: String -> (Token, String)
readAss s = case stripPrefix ":=" s of Just xs -> (OP_ASS, xs)
                                       Nothing -> error "Lexer error"

-- obrada svih ostalih tokena
-- OP_NOT OP_PLUS OP_MUL OP_GT OP_LT OP_EQ  SEMICOLON 
-- PAREN_LEFT PAREN_RIGHT BRKT_LEFT BRKT_RIGHT

readSign :: Char -> Token
readSign '!' = OP_NOT
readSign '+' = OP_PLUS
readSign '*' = OP_MUL
readSign '>' = OP_ORD GT
readSign '<' = OP_ORD LT
readSign '=' = OP_ORD EQ
readSign ';' = SEMICOLON
readSign '(' = PAREN_LEFT
readSign ')' = PAREN_RIGHT
readSign '{' = BRKT_LEFT
readSign '}' = BRKT_RIGHT
readSign c = error "Lexer error"