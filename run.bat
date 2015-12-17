REM batch skripta koja pokrece kompajler, VSM, i generise sliku sintaksnog drveta

compiler
VSM
ast_to_dot
dot -Tpng out/ast.dot -o out/ast.png
pause