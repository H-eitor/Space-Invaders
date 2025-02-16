Códigos no terminal:
cabal install --lib gloss --package-env .
cabal install --lib containers --package-env .
cabal install --lib random --package-env .
cabal install OpenGL GLUT
ghc -lglut -lGL -lGLU main.hs
