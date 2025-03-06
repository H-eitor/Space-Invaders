# SPACE INVADERS
Projeto na linguagem Haskell para a disciplina de PLP


## Resumo:
  O projeto implementa o jogo de arcade Space Invaders na linguagem funcional Haskel.
  
  O jogo original tem como seu objetivo o jogador atirar nos inimigos e ir pontuando a cada tiro acertado e inimigo eliminado    assim que todos os inimigos de uma tela são eliminados, aparecem mais inimigos para serem eliminados, na 5 rodada
  aparece o Chefão, que tem 10 vidas, diferente dos inimigos normais. O jogo é um ciclo com o objetivo do jogador obter a maior   pontuação possível.


## Passo a Passo para a intalação:
Para windows: 
- 1º Passo: clonar o diretorio do jogo, colando esse comando dentro do seu cmd
  - git clone https://github.com/H-eitor/Space-Invaders

- 2º Passo: Rodas os seguintes comandos no cmd estando dentro do diretorio do jogo
  - cabal install --lib gloss --package-env .
  - cabal install --lib containers --package-env .
  - cabal install --lib random --package-env .
  - cabal install --lib directory-1.3.7.1 --package-env .
  - cabal install --lib bytestring --package-env .
  - cabal install --lib csv --package-env .
  - cabal install --lib OpenGL GLUT --package-env .
  - ghc -lglut -lGL -lGLU main.hs
  
- 3º Passo: compilar o código do jogo
  - gch main.hs

- 4º Passo: executar o jogo diretamente pelo main.exe que foi criado apartir da compilação do código


## Grupo:
- Rafael Alencar Adelino de Lima - 123110785
- Isaque Esdras Rocha Cavalcante - 123110685
- Heitor de Souza Alves - 123110811
- Luiz da Costa Araujo Bronzeado Neto - 123110804
