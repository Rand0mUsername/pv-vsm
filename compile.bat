REM batch skripta koja kompajlira sve sors kodove

cd src\
g++ ast_to_dot.cpp -O2 -o ..\ast_to_dot -std=c++11
g++ VSM.cpp -O2 -o ..\VSM -std=c++11
ghc -o ..\compiler compiler.hs -outputdir ..\int\
pause