-- Para esse projeto foram utilizadas as bibliotecas: 
-- Gloss: permite realizar renderização grafica na tela e eventos para a interface do jogo (a parte grafica consiste
-- nos jogadores, inimigos, disparos e escudos poderem aparecer de forma grafica e a interface consiste em poder realizar eventos e
-- atualizar o estado do jogo, ou seja, apertar um botao e o jgaodr diparar ou se mover, ou levar 3 disparos e o jogo acabar); 
-- Random: usado para criar numeros aleatorios (foi usada para determinar se inimigos devem atirar, velocidade dos projeteis dos inimigos, velocidade dos inimigos, etc);
-- Data.Map: fornece uma implementação de mapas (dicionarios) para o codigo (é utilizado para associar por exemplo uma posição de linha e coluna a um inimigo)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as M
import System.Random (newStdGen, randoms)

-- DEFINIÇÃO DE TIPOS:

-- Representa o tipo de uma tupla formado por dois valores do tipo x
type Pair x = (x, x)

-- Tipo enumerado que representa o estado do jogo (se está rodando, se o jogador perdeu ou se esta na tela final )
data Status = Running | Lost | EnteringName deriving (Eq)

-- Tipo que representa qualquer entidade de jogo (jogador, inimigos, balas, etc)
data Item x = Item
    { _siz :: Pair x -- tamanho do item
    , _pos :: Pair x -- posição do item
    , _vel :: Pair x } deriving (Eq)  -- velocidade do item

-- Representa o estado completo do jogo, criando um tipo chamado Game que é parametrizado por x, permitindo que o jogo seja generico em relaçao ao tipo numerico usado 
-- esse tipo é manipulado durante o codigo para atualizar o estado do jogo, processar entrada do jogador, movimentar os objetos e verificar colisões 
data Game x = Game
    { _status :: Status -- status do jogo 
    , _inputLeft :: Bool -- indica que o jogador esta indo p esquerda
    , _inputRight :: Bool -- indica que o jogador esta indo p direita
    , _inputFire :: Bool -- indica que o jogador esta atirando
    , _rands :: [x] -- lista de numeros aleatorios 
    , _firetime :: x -- tempo desde o ultimo tiro do jogador
    , _player :: Item x -- representa o jogador (como ele é um Item X, tera as informações de sua posicao,velocidade, vida e tamanho)
    , _playerBullets :: [Item x]  -- lista de balas disparadas pelo jogador
    , _enemyBullets :: [Item x]   -- lista de balaras disparadas pelos inimigos
    , _shields :: [Item x] -- representa os escudos que aparecem na tela
    , _invaders :: [Item x] -- lista de inimigos na tela
    , _wave :: Int -- numero da fase que esta atualmente sendo executada no jogo
    , _score :: Int -- pontuação realizada pelo jogador
    , _lives :: Int -- quantidade de vidas restantes do jogador
    , _playerName :: String -- nome do jogador
    }

-- FUNCOES AUXILIARES:

-- Soma dois pares ordenados componente a componente
pSum :: Num a => Pair a -> Pair a -> Pair a
pSum (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

-- Subtrai dois pares ordenados componente a componente
pMinus :: Num a => Pair a -> Pair a -> Pair a
pMinus (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)

-- Multiplica dois pares ordenados componente a componente
pMult :: Num a => a -> Pair a -> Pair a
pMult k (x, y) = (k * x, k * y)

-- Inverte o sinal de cada componente de um par ordenado
pNegate :: Num a => Pair a -> Pair a
pNegate (x, y) = (negate x, negate y)


-- Objetos do jogo:

-- Função responsável por inicializar o estado do jogo, criando o jogador, os inimigos, os escudos e configurações variaveis essenciais
-- Parametros: rands0 (lista de numeros aleatorios) e wave (numero de fileira de inimigos da fase)
-- Saida: Game a (estado atual do jogo)
startGame :: (Fractional a, Ord a) => [a] -> Int -> Game a
startGame rands0 wave = Game Running False False False rands1 0 player [] [] shields invaders wave 0 3 ""
    where
        player = Item (70, 20) (0, -250) (0, 0) -- cria o jogador
        -- jogador tem tamanho (70,20) Está posicionado em (0, -250), indicando que ele começa na parte inferior da tela.
        -- Tem velocidade (0, 0), ou seja, inicialmente está parado.

        ([mag, dir], rands1) = splitAt 2 rands0
        vx = 150 -- velocidade horizontal dos inimigos

        invaders -- cria os inimigos
            | wave `mod` 5 == 0 = replicate 10 (Item (70, 20) (0, 150) (vx, 0))  -- cria um chefe caso o numero de fileiras de inimigos da fase fosse 5
            | otherwise = [Item (70, 20) 
                (fromIntegral x * 100, fromIntegral (-y) * 50 + 150)  -- cria as fileiras de inimigos
                (vx, fromIntegral wave * (-2))
                | x <- [-2..2], y <- [0..wave-1] ] -- list comprehension para definir a quantidade de inimigos criados por fileira (x) e o numero de fileiras (y)
                -- quanto mairo o numero de wave maior sera o numero de fileiras

        shields = [Item (70, 15) -- cria os escudos
            (fromIntegral x * 150, fromIntegral y * 15 - 180 )
            (0, 0)
            | x <- [-2..2], y <- [1..4]] -- list comprehension para definir o numero de escudos (x) e quantos escudos estao empilhados (y) (o empilhamento representa a vida dos escudos)

-- Função responsável por atualizar a posição do jogador com base nas entradas de _inputLeft e _inputRight
-- Parametros: time (numero fracionario, representa o tempo desde o ultimo frame), Game a (estado do jogo antes da atualização)
-- Saida: Game a (estado do jogo atualizado)
updatePlayer :: (Fractional a, Ord a) => a -> Game a -> Game a
updatePlayer time game = playerShoot time game1 
    where distance = time * 200 -- calcula a distancia que o jogador percorre para as entradas de direita ou esquerda 
          dleft = if _inputLeft game then -distance else 0 -- entrada para mover para esquerda
          dright = if _inputRight game then distance else 0 -- entrada para mover para direita
          (x0, y) = _pos (_player game) 
          x1 = max (-400) $ min 400 $ dleft + dright + x0  -- atualiza a nova posição horizontal do jogador
          game1 = game {_player = ((_player game) {_pos = (x1,y)})} 

-- Função responsável por atualizar a posição dos inimigos com base no tempo decorrido e também faz com que eles mudem de direção ao atingir as bordas da tela
-- Parametros: time (numero fracionario, representa o tempo percorrido desde o ultimo update), game (estado atual do jogo)
-- Saida: Game a (estado atualizado do jogo)
updateInvaders :: (Fractional a, Ord a) => a -> Game a -> Game a
updateInvaders time game = if null myInvaders then game else game3 -- se nao existe invasores retorna o jogo sem alteração 
    where myInvaders = _invaders game -- pega a lista de invasores 
          invader1 = map (autoUpdateItem time) myInvaders
          xs = map (fst . _pos) myInvaders
          x1min = minimum xs -- posição do inimigo mais a esquerda
          x1max = maximum xs -- posição do inimigo mais a direita
          (vx, vy) = _vel $ head myInvaders -- pega a velocidade do primeiro invasor 
          move v0 v1 invader = invader { _pos = pSum (_pos invader) (pMult time v0), _vel = v1 } 
          invader2 | vx>0 && x1max>380 = map (move (380-x1max, 0) (-vx, vy)) invader1 
             | vx<0 && x1min<(-380) = map (move (-380-x1min, 0) (-vx, vy)) invader1 
             | otherwise = invader1 
          game2 = game { _invaders = invader2 } 
          game3 = invaderShoot game2 

-- Função que move um item (como um jogador, um invasor ou um projétil) com base no tempo decorrido e na sua velocidade
-- Parametros: time (numero fracionario, representa o tempo percorrido desde o ultimo update), item@...(um item do jogo)
-- Saida: mesmo item porém com a posição atualizada
autoUpdateItem :: (Num a) => a -> Item a -> Item a
autoUpdateItem time item@(Item _ pos vel) = item { _pos = pSum pos (pMult time vel)}

-- Disparos:
-- Função responsável por gerenciar o disparo de projéteis pelo jogador, com base no tempo percorrido e na recarga do disparo
-- Parametros: time (numero fracionario, representa o tempo percorrido desde o ultimo update), game (estado atual do jogo)
-- Saida: Game a (estado atualizado do jogo)
playerShoot :: (Fractional a, Ord a) => a -> Game a -> Game a
playerShoot time game = if canFire then mFire else mNofire
    where canFire = _inputFire game && _firetime game > 0.9 -- verifica se o jogador pressionou o botao de disparo e se o tempo de recarga é maior que 0.9
          (x, y) = _pos (_player game) -- posição do jogador 
          bullet = Item (3, 15) (x, y+20) (0, 800) -- criação do projétil 
          mFire = game { _playerBullets = bullet : _playerBullets game, _firetime = 0 } 
          mNofire = game { _firetime = time + _firetime game } 

-- Função responsável por gerenciar os disparos de projéteis feitos pelos inimigos
-- Parametro: game (estado atual do jogo)
-- Saida: Game a (estado atualizado do jogo)
invaderShoot :: (Fractional a, Ord a) => Game a -> Game a
invaderShoot game = game { _enemyBullets = _enemyBullets game ++ bulletList, _rands = rands3 } -- tiros dos inimigos são adicionados à lista de balas do jogo
    where invadersPos = map _pos $ _invaders game -- gera uma lista com a posição de todos os inimigos da fase atual do jogo
          fInsert pMap (x,y) = M.insertWith min x y pMap
          fighters0 = M.toList $ foldl fInsert M.empty invadersPos -- gera uma lista de tuplas de posições dos inimigos
          (rands0, rands1) = splitAt (length fighters0) (_rands game) 
          (rands2, rands3) = splitAt (length fighters0) rands1 
          
          difficulty -- dificuldade das fases (basicamente quanto o numero de fileiras/waves maior será a velocidade que os inimigos vao atirar)
            | _wave game == 5 = 0.92
            | _wave game == 4 = 0.98
            | _wave game == 3 = 0.983
            | otherwise = 0.992
          
          fighters1 = [ (p, vel) | (p, r, vel) <- zip3 fighters0 rands0 rands2, r > difficulty ] -- list comprehension que contem os inimigos que irão atirar, com a posição dos inimigis
          -- e com conjuntos de numeros aleatorios, o filtro assegura que apenas inimigos com um numero aleatorio maior que o valor da dificuldade irão atirar
          
          createBullet ((x, y), vel) = Item (3, 9) (x, y-20) (0, -(300-vel*200)) -- cria a bala dos inimigos 
          
          bulletList = map createBullet fighters1 -- cria uma lista de balas

-- Função responsável por atualizar as balas do jogador e dos inimigos, garantindo que elas se movam de acordo com o tempo e removendo as balas que saem da tela
-- Parametros: time (numero fracionario, representa o tempo percorrido desde o ultimo update), game (estado atual do jogo)
-- Saida: Game a (estado atualizado do jogo)
updateBullets :: (Num a, Ord a) => a -> Game a -> Game a
updateBullets time game = game { _playerBullets = playerb2, _enemyBullets = enemyb2 }
    where playerb1 = map (autoUpdateItem time) (_playerBullets game) 
          playerb2 = filter (\bull -> snd (_pos bull) < 300 && snd (_pos bull) > -300) playerb1 
          enemyb1 = map (autoUpdateItem time) (_enemyBullets game) 
          enemyb2 = filter (\bull -> snd (_pos bull) < 300 && snd (_pos bull) > -300) enemyb1 

-- Colisões
-- Função responsável por detectar e processar as colisões entre diferentes elementos do jogo, como as balas do jogador, as balas dos inimigos, os invasores e os escudos.
-- Parametros: game (estado atual do jogo)
-- Saida: Game a (estado atualizado do jogo)
updateCollisions :: (Fractional a, Ord a) => Game a -> Game a
updateCollisions game = game1 { _playerBullets = playerb2, _enemyBullets = enemyb2, _shields = shield3, _invaders = invader1, _score = newScore }
    where 
        (playerb1, invader1) = runCollisions (_playerBullets game) (_invaders game)  -- verifica colisões das balas do jogador com os invasores
        (enemyb1, player1) = runCollisions (_enemyBullets game) [_player game] -- verifica colisões das balas dos inimigos com o jogador
        (playerb2, shield1) = runCollisions playerb1 (_shields game) -- verifica colisões das balas do jogador com os escudos
        (enemyb2, shield2) = runCollisions enemyb1 (_shields game) -- verifica colisões das balas dos inimigos com os escudos
        (_, shield3) = runCollisions invader1 shield2 -- verifica coliões dos inimigos com os escudos
        (_, player2) = runCollisions invader1 player1 -- verifica colisões dos inimigos com o jogador

        pontosPorInimigo = if _wave game `mod` 5 == 0 then 10 else 1 -- quantidade de pontos que cada inimigo gera ao serem derrotados, se a fase atual for a 5, ou seja a fase do chefe, os inimigos darão mais pontos
        newScore = _score game + (length (_invaders game) - length invader1) * pontosPorInimigo
        newLives -- atualiza a vida do jogador
          | player2 /= player1 = 0
          | null player1 = _lives game - 1
          | otherwise = _lives game

        status = if newLives <= 0 then Lost else Running -- se o jogador nao tem mais vida entao ele perde o jogo
        game1 = if status == Lost then game { _status = Lost, _lives = 0 } else game { _lives = newLives, _score = newScore } -- atualização do jogo


testCollision :: (Fractional a, Ord a) => Item a -> Item a -> Bool
testCollision (Item as ap _) (Item bs bp _) =
    ((bx0 < ax0 && ax0 < bx1) || (bx0 < ax1 && ax1 < bx1)) &&
    ((by0 < ay0 && ay0 < by1) || (by0 < ay1 && ay1 < by1))
    where (ax0, ay0) = pMinus ap (pMult 0.5 as) 
          (ax1, ay1) = pSum ap (pMult 0.5 as)
          (bx0, by0) = pMinus bp (pMult 0.5 bs)
          (bx1, by1) = pSum bp (pMult 0.5 bs)

runCollisions :: (Fractional a, Ord a) => [Item a] -> [Item a] -> ([Item a], [Item a])
runCollisions [] is = ([], is)
runCollisions (b:bs) [] = (b:bs, [])  -- Se não há inimigos, mantém as balas
runCollisions (b:bs) is =
    case break (testCollision b) is of
        (_, []) -> let (bs', is') = runCollisions bs is
                   in (b:bs', is')  -- Nenhum inimigo atingido, mantém a bala
        (before, _:after) -> let (bs', is') = runCollisions bs (before ++ after)
                             in (bs', is')  -- Remove apenas um inimigo atingido

step :: (Fractional a, Ord a) => a -> Game a -> Game a
step time g
    | _status g /= Running = g  -- Se o jogo não está rodando, não faz nada
    | null (_invaders g) = startNewWave g  -- Avança para a próxima wave se não houver invasores
    | otherwise = updateGame time g  -- Atualiza o estado do jogo
    where
        updateGame time g = updatePlayer time
                            $ updateInvaders time 
                            $ updateBullets time
                            $ updateCollisions g

        -- Função para iniciar uma nova wave
        startNewWave g = 
            let newWave = if _wave g == 5 then 1 else _wave g + 1  
                newGame = startGame (_rands g) newWave
            in newGame { _score = _score g, _lives = _lives g }  


-- MAIN

saveScore :: Game Float -> IO ()
loadScores :: IO [String]
-- Salva nome e pontuação do jogador
saveScore g = do
    scores <- readFile "scores.txt"
    let newScore = _playerName g ++ " - Score: " ++ show (_score g)
    let updatedScores = newScore ++ "\n" ++ scores
    writeFile "scores.txt" updatedScores

loadScores = do
    content <- readFile "scores.txt"
    return $ lines content

displayH :: Game Float -> Picture
displayH g = case _status g of
    Lost -> pictures [gameOverText "GAME OVER!", continueText "Press [F1] to Continue | press [ENTER] to save score", scoreText, livesText]
    EnteringName -> pictures [enterNameText "ENTER YOUR NAME:", nameText, continueText "Press [ENTER] to save"]
    _    -> pictures $ scoreText : livesText : player : playerBullets ++ enemyBullets ++ shields ++ invaders
    where 
        player = drawItem green (_player g)
        playerBullets = map (drawItem yellow) (_playerBullets g)
        enemyBullets = map (drawItem orange) (_enemyBullets g)
        shields = map (drawItem blue) (_shields g)
        invaders = map (drawItem red) (_invaders g)

        scoreText = Color white $ Translate (-380) 260 $ Scale 0.3 0.3 $ Text ("Score: " ++ show (_score g))
        livesText = Color white $ Translate (-380) (-280) $ Scale 0.3 0.3 $ Text ("Lives: " ++ show (_lives g))
        gameOverText msg = Color white $ Translate (-200) 50 $ Scale 0.5 0.5 $ Text msg
        continueText msg = Color white $ Translate (-200) 10 $ Scale 0.1 0.1 $ Text msg
        enterNameText msg = Color white $ Translate (-200) 30 $ Scale 0.2 0.2 $ Text msg
        nameText = Color white $ Translate 90 30 $ Scale 0.2 0.2 $ Text (_playerName g)

drawItem :: Color -> Item Float -> Picture
drawItem c it = Color c $ Translate x y $ rectangleSolid sx sy
    where (x, y) = _pos it
          (sx, sy) = _siz it

eventH :: Event -> Game Float -> Game Float
eventH (EventKey (SpecialKey KeyLeft) Down _ _)  g = g { _inputLeft = True }
eventH (EventKey (SpecialKey KeyLeft) Up _ _)    g = g { _inputLeft = False }
eventH (EventKey (SpecialKey KeyRight) Down _ _) g = g { _inputRight = True }
eventH (EventKey (SpecialKey KeyRight) Up _ _)   g = g { _inputRight = False }
eventH (EventKey (SpecialKey KeySpace) Down _ _) g = g { _inputFire = True }
eventH (EventKey (SpecialKey KeySpace) Up _ _)   g = g { _inputFire = False }
--TODO tem alguma coisa errada aqui, já que sempre que o jogador pressiona enter o jogo aparece por uma frame
eventH (EventKey (SpecialKey KeyEnter) Down _ _) g
  | _status g == Lost = g { _status = EnteringName }
  | _status g == EnteringName = g { _status = Running, _playerName = "" } -- Salva a pontuação e reinicia o jogo
  where _ = saveScore g
eventH (EventKey (SpecialKey f1) Down _ _) g
  | _status g == Lost = restartGame g 
-- Função de inserir nome
eventH (EventKey (Char c) Down _ _) g
  | _status g == EnteringName = g { _playerName = _playerName g ++ [c] }
-- Função para apagar o nome ao pressionar Backspace
eventH (EventKey (SpecialKey KeyBackspace) Down _ _) g
  | _status g == EnteringName = g { _playerName = init (_playerName g) }

eventH _ g = g

idleH :: Float -> Game Float -> Game Float
idleH = step

restartGame :: Game Float -> Game Float 
restartGame g = startGame (_rands g) 1 -- reinicia no primeiro nivel

main :: IO ()
main = do
    myRands <- randoms <$> newStdGen
    let myGame = startGame myRands 1
    let myWindow = InWindow "haskell-invaders" (800, 600) (0, 0)
    play myWindow black 30 myGame displayH eventH idleH
