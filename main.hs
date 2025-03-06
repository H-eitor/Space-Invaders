-- Para esse projeto foram utilizadas as bibliotecas: 
-- Gloss: permite realizar renderização grafica na tela e eventos para a interface do jogo (a parte grafica consiste
-- nos jogadores, inimigos, disparos e escudos poderem aparecer de forma grafica e a interface consiste em poder realizar eventos e
-- atualizar o estado do jogo, ou seja, apertar um botao e o jgaodr diparar ou se mover, ou levar 3 disparos e o jogo acabar); 
-- Random: usado para criar numeros aleatorios (foi usada para determinar se inimigos devem atirar, velocidade dos projeteis dos inimigos, velocidade dos inimigos, etc);
-- Data.Map: fornece uma implementação de mapas (dicionarios) para o codigo (é utilizado para associar por exemplo uma posição de linha e coluna a um inimigo)
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use Down" #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as M
import System.Random (newStdGen, randoms)
import System.IO
import System.Directory
import Text.CSV
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import System.IO.Unsafe
import Data.List (sortBy, intercalate)
import Data.Ord (comparing)


-- DEFINIÇÃO DE TIPOS:

-- Representa o tipo de uma tupla formado por dois valores do tipo x
type Pair x = (x, x)

-- Tipo enumerado que representa o estado do jogo (se está rodando, se o jogador perdeu ou se esta na tela final )
data Status = Running | Lost | EnteringName deriving (Eq)

-- Tipo que representa qualquer entidade de jogo (jogador, inimigos, balas, etc)
data Item x = Item
    { _siz :: Pair x
    , _pos :: Pair x
    , _vel :: Pair x } deriving (Eq)

-- Representa o estado completo do jogo, criando um tipo chamado Game que é parametrizado por x, permitindo que o jogo seja generico em relaçao ao tipo numerico usado 
-- esse tipo é manipulado durante o codigo para atualizar o estado do jogo, processar entrada do jogador, movimentar os objetos e verificar colisões 
data Game x = Game
    { _status :: Status
    , _inputLeft :: Bool
    , _inputRight :: Bool
    , _inputFire :: Bool
    , _rands :: [x]
    , _firetime :: x
    , _player :: Item x
    , _playerBullets :: [Item x]
    , _enemyBullets :: [Item x]
    , _shields :: [Item x]
    , _invaders :: [Item x]
    , _wave :: Int
    , _score :: Int
    , _lives :: Int
    , _playerName :: String }

data Player = Player
    { nickname :: String
    , highscore :: Int } deriving (Show, Generic)

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
        player = Item (70, 20) (0, -250) (0, 0)

        ([mag, dir], rands1) = splitAt 2 rands0
        vx = 150

        invaders
            | wave `mod` 5 == 0 = replicate 10 (Item (70, 20) (0, 150) (vx, 0))
            | otherwise = [Item (70, 20)
                (fromIntegral x * 100, fromIntegral (-y) * 50 + 150)
                (vx, fromIntegral wave * (-2))
                | x <- [-2..2], y <- [0..wave-1] ]

        shields = [Item (70, 15)
            (fromIntegral x * 150, fromIntegral y * 15 - 180 )
            (0, 0)
            | x <- [-2..2], y <- [1..4]]

-- Função responsável por atualizar a posição do jogador com base nas entradas de _inputLeft e _inputRight
-- Parametros: time (numero fracionario, representa o tempo desde o ultimo frame), Game a (estado do jogo antes da atualização)
-- Saida: Game a (estado do jogo atualizado)
updatePlayer :: (Fractional a, Ord a) => a -> Game a -> Game a
updatePlayer time game = playerShoot time game1
    where distance = time * 200
          dleft = if _inputLeft game then -distance else 0
          dright = if _inputRight game then distance else 0
          (x0, y) = _pos (_player game)
          x1 = max (-400) $ min 400 $ dleft + dright + x0
          game1 = game {_player = ((_player game) {_pos = (x1,y)})}

-- Função responsável por atualizar a posição dos inimigos com base no tempo decorrido e também faz com que eles mudem de direção ao atingir as bordas da tela
-- Parametros: time (numero fracionario, representa o tempo percorrido desde o ultimo update), game (estado atual do jogo)
-- Saida: Game a (estado atualizado do jogo)
updateInvaders :: (Fractional a, Ord a) => a -> Game a -> Game a
updateInvaders time game = if null myInvaders then game else game3
    where myInvaders = _invaders game
          invader1 = map (autoUpdateItem time) myInvaders
          xs = map (fst . _pos) myInvaders
          x1min = minimum xs
          x1max = maximum xs
          (vx, vy) = _vel $ head myInvaders
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
    where canFire = _inputFire game && _firetime game > 0.9
          (x, y) = _pos (_player game)
          bullet = Item (3, 15) (x, y+20) (0, 800)
          mFire = game { _playerBullets = bullet : _playerBullets game, _firetime = 0 }
          mNofire = game { _firetime = time + _firetime game }

-- Função responsável por gerenciar os disparos de projéteis feitos pelos inimigos
-- Parametro: game (estado atual do jogo)
-- Saida: Game a (estado atualizado do jogo)
invaderShoot :: (Fractional a, Ord a) => Game a -> Game a
invaderShoot game = game { _enemyBullets = _enemyBullets game ++ bulletList, _rands = rands3 }
    where invadersPos = map _pos $ _invaders game
          fInsert pMap (x,y) = M.insertWith min x y pMap
          fighters0 = M.toList $ foldl fInsert M.empty invadersPos
          (rands0, rands1) = splitAt (length fighters0) (_rands game)
          (rands2, rands3) = splitAt (length fighters0) rands1

          difficulty
            | _wave game == 5 = 0.92
            | _wave game == 4 = 0.98
            | _wave game == 3 = 0.983
            | otherwise = 0.992

          fighters1 = [ (p, vel) | (p, r, vel) <- zip3 fighters0 rands0 rands2, r > difficulty ]
          createBullet ((x, y), vel) = Item (3, 9) (x, y-20) (0, -(300-vel*200))

          bulletList = map createBullet fighters1

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
        (playerb1, invader1) = runCollisions (_playerBullets game) (_invaders game)
        (enemyb1, player1) = runCollisions (_enemyBullets game) [_player game]
        (playerb2, shield1) = runCollisions playerb1 (_shields game)
        (enemyb2, shield2) = runCollisions enemyb1 (_shields game)
        (_, shield3) = runCollisions invader1 shield2
        (_, player2) = runCollisions invader1 player1

        pontosPorInimigo = if _wave game `mod` 5 == 0 then 10 else 1
        newScore = _score game + (length (_invaders game) - length invader1) * pontosPorInimigo
        newLives
          | player2 /= player1 = 0
          | null player1 = _lives game - 1
          | otherwise = _lives game

        status = if newLives <= 0 then Lost else Running
        game1 = if status == Lost then game { _status = Lost, _lives = 0 } else game { _lives = newLives, _score = newScore }

-- Função responsável por verificar se dois items colidem entre si
-- Tem como entrada dois itens que serão testados se colidem
-- Tem como saida um valor booleano que representa se existe ou nao a colisão
testCollision :: (Fractional a, Ord a) => Item a -> Item a -> Bool
testCollision (Item as ap _) (Item bs bp _) =
    ((bx0 < ax0 && ax0 < bx1) || (bx0 < ax1 && ax1 < bx1)) &&
    ((by0 < ay0 && ay0 < by1) || (by0 < ay1 && ay1 < by1))
    where (ax0, ay0) = pMinus ap (pMult 0.5 as)
          (ax1, ay1) = pSum ap (pMult 0.5 as)
          (bx0, by0) = pMinus bp (pMult 0.5 bs)
          (bx1, by1) = pSum bp (pMult 0.5 bs)

-- Função que gerencia colisões entre duas listas de itens, removendo itens que estao colidindo
-- Recebe duas listas de itens e retorna um par de listas modificadas
runCollisions :: (Fractional a, Ord a) => [Item a] -> [Item a] -> ([Item a], [Item a])
runCollisions [] list = ([], list)
runCollisions (b:bs) [] = (b:bs, [])
runCollisions (b:bs) list =
    case break (testCollision b) list of
        (_, []) -> let (bs', list') = runCollisions bs list
                   in (b:bs', list')
        (before, _:after) -> let (bs', list') = runCollisions bs (before ++ after)
                             in (bs', list')

-- Função responsável por controlar a atualização do jogo a cada intervalo de tempo.
-- Parametros: time (numero fracionario, representa o tempo percorrido desde o ultimo update), game (estado atual do jogo)
-- Saida: Game a (estado atualizado do jogo)
step :: (Fractional a, Ord a) => a -> Game a -> Game a
step time game
    | _status game /= Running = game
    | null (_invaders game) = startNewWave game
    | otherwise = updateGame time game
    where
        updateGame time game = updatePlayer time
                            $ updateInvaders time
                            $ updateBullets time
                            $ updateCollisions game
        startNewWave game =
            let newWave = if _wave game == 5 then 1 else _wave game + 1
                newGame = startGame (_rands game) newWave
            in newGame { _score = _score game, _lives = _lives game }


-- MAIN

-- Salva nome e pontuação do jogador
saveScore :: Game Float -> IO ()
saveScore g =  do
    savePlayerCSV (_playerName g) (_score g)
    orderCSV

displayH :: Game Float -> Int -> Picture
displayH g highScore = case _status g of
    Lost -> pictures [gameOverText "GAME OVER!", continueText "Press [F1] to Continue | press [ENTER] to save score", scoreText, livesText, highScoreText]
    EnteringName -> pictures [enterNameText "ENTER YOUR NAME:", nameText, continueText "Press [ENTER] to save"]
    _    -> pictures $ scoreText : livesText : highScoreText : player : playerBullets ++ enemyBullets ++ shields ++ invaders
    where
        player = drawItem green (_player g)
        playerBullets = map (drawItem yellow) (_playerBullets g)
        enemyBullets = map (drawItem orange) (_enemyBullets g)
        shields = map (drawItem blue) (_shields g)
        invaders = map (drawItem red) (_invaders g)

        scoreText = Color white $ Translate (-380) 260 $ Scale 0.3 0.3 $ Text ("Score: " ++ show (_score g))
        livesText = Color white $ Translate (-380) (-280) $ Scale 0.3 0.3 $ Text ("Lives: " ++ show (_lives g))
        highScoreText = Color white $ Translate 130 260 $ Scale 0.3 0.3 $ Text ("Hi-Score: " ++ show highScore)
        gameOverText msg = Color white $ Translate (-200) 50 $ Scale 0.5 0.5 $ Text msg
        continueText msg = Color white $ Translate (-200) 10 $ Scale 0.1 0.1 $ Text msg
        enterNameText msg = Color white $ Translate (-200) 30 $ Scale 0.2 0.2 $ Text msg
        nameText = Color white $ Translate 90 30 $ Scale 0.2 0.2 $ Text (_playerName g)

-- Função responsável por desenhar um objeto na tela do jogo
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
eventH (EventKey (SpecialKey KeyEnter) Down _ _) g
  | _status g == Lost = g { _status = EnteringName }
  | _status g == EnteringName = unsafePerformIO $ do
      saveScore g
      return $ g { _status = Running, _playerName = "" } 
eventH (EventKey (SpecialKey KeyF1) Down _ _) g
  | _status g == Lost = restartGame g
eventH (EventKey (Char c) Down _ _) g
  | _status g == EnteringName = g { _playerName = _playerName g ++ [c] }
eventH (EventKey (SpecialKey KeyBackspace) Down _ _) g
  | _status g == EnteringName = g { _playerName = init (_playerName g) }
eventH _ g = g

-- Função responsável por atualizar o jogo mesmo que o jogador nao esteja interagindo com jogo
idleH :: Float -> Game Float -> Game Float
idleH = step

-- Função responsavel por reiniciar o jogo na fase 1
restartGame :: Game Float -> Game Float
restartGame g = startGame (_rands g) 1 -- reinicia no primeiro nivel

-- Main que inicia todo o codigo
main :: IO ()
main = do
    highScore <- loadHighScore
    myRands <- randoms <$> newStdGen
    let myGame = startGame myRands 1
    let myWindow = InWindow "haskell-invaders" (800, 600) (0, 0)
    play myWindow black 30 myGame (`displayH` highScore) eventH idleH

toInt :: String -> Int
toInt s = read s :: Int

getPlayerCSV :: [Record]
getPlayerCSV = do
    let file = unsafePerformIO ( readFile "placar.csv" )
    let csvFile = parseCSV "placar.csv" file
    either csvParseError csvParseDone csvFile
    where csvParseError csvFile = []
          csvParseDone = tail

recordToPlayer :: [Record] -> [Player]
recordToPlayer [] = []
recordToPlayer (x:xs) = do
    let p = Player (head x) (toInt $ x!!1)
    p : recordToPlayer xs

playerToCSV :: String -> Int -> String
playerToCSV name score = "\n" ++ name ++ "," ++ show score

playerObjToCSV :: Player -> String
playerObjToCSV p = "\n" ++ nickname p ++ "," ++ show (highscore p)

savePlayerCSV :: String -> Int -> IO()
savePlayerCSV name score = do
    let p = playerToCSV name score
    f <- openFile "placar.csv" AppendMode
    hPutStr f p
    hClose f

playerListToCSV :: [Player] -> String
playerListToCSV = concatMap playerObjToCSV

orderCSV :: IO()
orderCSV = do
    let playerList = recordToPlayer getPlayerCSV
    let playerListOrd = take 5 $ sortBy (flip $ comparing highscore) playerList
    let top5 = take 5 playerListOrd
    B.writeFile "Temp.csv" $ BC.pack ("name,score" ++ playerListToCSV top5)
    removeFile "placar.csv"
    renameFile "Temp.csv" "placar.csv"

loadHighScore :: IO Int
loadHighScore = do
    exists <- doesFileExist "placar.csv"
    if exists
        then do
            contents <- readFile "placar.csv"
            let csv = parseCSV "placar.csv" contents
            case csv of
                Right records -> do
                    let scores = map (read . (!!1)) (tail records) :: [Int]
                    if null scores
                        then return 0
                        else return (maximum scores)
                Left _ -> return 0
        else return 0

