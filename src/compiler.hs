{-
    Nikola Jovanovic - Matematicka gimnazija 4D
    Maturski rad iz informatike: 
    Lambda racun i funkcionalno programiranje 
    uz dodatni projekat : izrada kompajlera u programskom jeziku Haskell
    4.2015.
-}

import Data.Char 
import Data.List
import Lexer
import Parser
import Codegen
import System.IO 
import System.Directory 

{-
   Sam kompajler, koji koristi prethodno ucitane komponente 
   (lekser, parser, kod generator)
-}

-- ceo pipeline za kompilaciju
-- trenutno se ne koristi jer zelimo da vidimo i medjurezultate kompilacije

compile :: String -> String
compile program = codegen . parse . lexx $ program

-- IO
main = do     

    -- ucitavanje ulaznog programa u jeziku PV
    inHandle <- openFile "in/program.pv" ReadMode
    program <- hGetContents inHandle

    -- priprema fajlova za ispis rezultata pojedinacnih komponenti
    -- i zavrsnog izlaznog VSM programa
    tokensHandle <- openFile "out/tokens.txt" WriteMode
    astHandle <- openFile "out/ast.txt" WriteMode
    outHandle <- openFile "out/program.vsm" WriteMode

    -- leksicka analiza
    let tokens = lexx program 
    hPutStr tokensHandle $ show tokens 
    hClose tokensHandle 

    -- parsiranje
    let ast = parse tokens
    hPutStr astHandle $ show ast     
    hClose astHandle 

    -- generisanje koda
    let vsmProg = codegen ast -- <=> compile program
    hPutStr outHandle vsmProg  
    hClose outHandle

    -- zatvaranje input fajla se mora vrsiti na kraju
    -- zbog lenjosti hGetContents komande
    hClose inHandle   