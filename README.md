# SPACE INVADERS
Projeto na linguagem Haskell para a disciplina de PLP

![Alt text](https://res.cloudinary.com/cook-becker/image/fetch/q_auto:best,f_auto,w_1920,g_center/https://candb.com/site/candb/images/artwork/MarqueeHome.jpg "title image")

## Resumo:
  O projeto implementa o jogo de arcade Space Invaders na linguagem funcional Haskel.
  
  O jogo original tem como seu objetivo o jogador atirar nos inimigos e ir pontuando a cada tiro acertado e inimigo eliminado    assim que todos os inimigos de uma tela são eliminados, aparecem mais inimigos para serem eliminados, na 5 rodada
  aparece o Chefão, que tem 10 vidas, diferente dos inimigos normais. O jogo é um ciclo com o objetivo do jogador obter a maior   pontuação possível.


## Passo a Passo para a intalação:

- 1º Passo: clonar o diretorio do jogo.
  - git clone https://github.com/H-eitor/Space-Invaders

- 2º Passo: Rodar os seguintes comandos no terminal estando dentro do diretorio do jogo para instalar as bibliotecas usadas.
  - cabal install --lib gloss --package-env .
  - cabal install --lib containers --package-env .
  - cabal install --lib random --package-env .
  - cabal install --lib directory-1.3.7.1 --package-env .
  - cabal install --lib bytestring --package-env .
  - cabal install --lib csv --package-env .
  - cabal install --lib OpenGL GLUT --package-env .

-> Caso esteja no linux ou algum derivado, pode ser necessário instalar o pacote freeglut usando o seu package manager, neste caso:
  - [arch linux] sudo pacman -S freeglut 
  - [Mint, Debian, Ubuntu, ElementaryOs] sudo apt-get install freeglut3-dev

- 3º Passo: compilar o código do jogo
  - ghc main.hs
-> pode ser que o comando anterior não compile corretamente dependendo do sistema operacional, neste caso tente expor as bibliotecas, exemplos:
  - ghc -package gloss main.hs 
  - ghc -package gloss -package directories main.hs

- 4º Passo: executar o jogo diretamente pelo executável que foi criado apartir da compilação do código

- IMPORTANTE: Para evitar qualquer erro é recomendável desativar o Windows Defender (não existe nenhuma ameaça no executavel mas alguns computadores podem reclamar)


## Grupo:
- Rafael Alencar Adelino de Lima - 123110785
- Isaque Esdras Rocha Cavalcante - 123110685
- Heitor de Souza Alves - 123110811
- Luiz da Costa Araujo Bronzeado Neto - 123110804
