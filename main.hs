import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as M
import System.Random (newStdGen, randoms)


-- TIPOS
type Pair x = (x, x)

data Status = Running | Lost | EnteringName deriving (Eq)

data Item x = Item
    { _siz :: Pair x
    , _pos :: Pair x
    , _vel :: Pair x} deriving (Eq)

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
    , _playerName :: String
    }

-- FUNCOES AUXILIARES
    -- Pares ordenados
pSum :: Num a => Pair a -> Pair a -> Pair a
pSum (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

pMinus :: Num a => Pair a -> Pair a -> Pair a
pMinus (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)

pMult :: Num a => a -> Pair a -> Pair a
pMult k (x, y) = (k * x, k * y)

pNegate :: Num a => Pair a -> Pair a
pNegate (x, y) = (negate x, negate y)


    -- Objetos

startGame :: (Fractional a, Ord a) => [a] -> Int -> Game a
startGame rands0 wave = Game Running False False False rands1 0 player [] [] shields invaders wave 0 3 ""
    where
        player = Item (70, 20) (0, -250) (0, 0)
        ([mag, dir], rands1) = splitAt 2 rands0
        vx = 150

        -- Lógica para criar os invasores
        invaders
            | wave `mod` 5 == 0 = replicate 10 (Item (70, 20) (0, 150) (vx, 0))  -- Chefe: 10 inimigos sobrepostos
            | otherwise = [Item (70, 20) 
                (fromIntegral x * 100, fromIntegral (-y) * 50 + 150)  -- Inimigos normais
                (vx, fromIntegral wave * (-2))
                | x <- [-2..2], y <- [0..wave-1] ]

        -- Escudos
        shields = [Item (70, 15)
            (fromIntegral x * 150, fromIntegral y * 15 - 180 )
            (0, 0)
            | x <- [-2..2], y <- [1..4]]

updatePlayer :: (Fractional a, Ord a) => a -> Game a -> Game a
updatePlayer time g = playerShoot time g1
    where distance = time * 200
          dl = if _inputLeft g then -distance else 0
          dr = if _inputRight g then distance else 0
          (x0, y) = _pos (_player g)
          x1 = max (-400) $ min 400 $ dl + dr + x0
          g1 = g {_player = ((_player g) {_pos = (x1,y)})}

updateInvaders :: (Fractional a, Ord a) => a -> Game a -> Game a
updateInvaders time g = if null myInvaders then g else g3
    where myInvaders = _invaders g
          i1 = map (autoUpdateItem time) myInvaders
          xs = map (fst . _pos) myInvaders
          x1min = minimum xs
          x1max = maximum xs
          (vx, vy) = _vel $ head myInvaders
          move v0 v1 i = i { _pos = pSum (_pos i) (pMult time v0), _vel = v1 } 
          i2 | vx>0 && x1max>380 = map (move (380-x1max, 0) (-vx, vy)) i1
             | vx<0 && x1min<(-380) = map (move (-380-x1min, 0) (-vx, vy)) i1
             | otherwise = i1
          g2 = g { _invaders = i2 }
          g3 = invaderShoot g2

autoUpdateItem :: (Num a) => a -> Item a -> Item a
autoUpdateItem t i@(Item _ pos vel) = i { _pos = pSum pos (pMult t vel)}

    -- Disparos

playerShoot :: (Fractional a, Ord a) => a -> Game a -> Game a
playerShoot time g = if canFire then mFire else mNofire
    where canFire = _inputFire g && _firetime g > 0.9
          (x, y) = _pos (_player g)
          bullet = Item (3, 15) (x, y+20) (0, 800)
          mFire = g { _playerBullets = bullet : _playerBullets g, _firetime = 0 }
          mNofire = g { _firetime = time + _firetime g }

invaderShoot :: (Fractional a, Ord a) => Game a -> Game a
invaderShoot g = g { _enemyBullets = _enemyBullets g ++ bs, _rands = rands3 }
    where invadersPos = map _pos $ _invaders g
          fInsert pMap (x,y) = M.insertWith min x y pMap
          fighters0 = M.toList $ foldl fInsert M.empty invadersPos
          (rands0, rands1) = splitAt (length fighters0) (_rands g)
          (rands2, rands3) = splitAt (length fighters0) rands1
          
          difficulty
            | _wave g == 5 = 0.92
            | _wave g == 4 = 0.98
            | _wave g == 3 = 0.983
            | otherwise = 0.992
          
          fighters1 = [ (p, v) | (p, r, v) <- zip3 fighters0 rands0 rands2, r > difficulty ]
          
          createBullet ((x, y), v) = Item (3, 9) (x, y-20) (0, -(300-v*200))
          
          bs = map createBullet fighters1

updateBullets :: (Num a, Ord a) => a -> Game a -> Game a
updateBullets time g = g { _playerBullets = pb2, _enemyBullets = eb2 }
    where pb1 = map (autoUpdateItem time) (_playerBullets g)
          pb2 = filter (\b -> snd (_pos b) < 300 && snd (_pos b) > -300) pb1
          eb1 = map (autoUpdateItem time) (_enemyBullets g)
          eb2 = filter (\b -> snd (_pos b) < 300 && snd (_pos b) > -300) eb1

    -- Colisões
updateCollisions :: (Fractional a, Ord a) => Game a -> Game a
updateCollisions g = g1 { _playerBullets = pb2, _enemyBullets = eb2, _shields = s3, _invaders = i1, _score = newScore }
    where 
        -- Verifica colisões das balas do jogador com os invasores
        (pb1, i1) = runCollisions (_playerBullets g) (_invaders g)
        
        -- Verifica colisões das balas dos inimigos com o jogador
        (eb1, p1) = runCollisions (_enemyBullets g) [_player g]
        
        -- Verifica colisões das balas do jogador com os escudos
        (pb2, s1) = runCollisions pb1 (_shields g)
        
        -- Verifica colisões das balas dos inimigos com os escudos
        (eb2, s2) = runCollisions eb1 (_shields g)

        -- Verifica coliões dos inimigos com os escudos
        (_, s3) = runCollisions i1 s2

        -- Verifica colisões dos inimigos com o jogador
        (_, p2) = runCollisions i1 p1

        -- Calcula a pontuação baseada na fase atual
        pontosPorInimigo = if _wave g `mod` 5 == 0 then 10 else 1
        newScore = _score g + (length (_invaders g) - length i1) * pontosPorInimigo
        newLives
          | p2 /= p1 = 0
          | null p1 = _lives g - 1
          | otherwise = _lives g

        -- Se não houver vidas restantes, altera o status para 'Lost'
        st = if newLives <= 0 then Lost else Running
        g1 = if st == Lost then g { _status = Lost, _lives = 0 } else g { _lives = newLives, _score = newScore }


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
    Lost -> pictures [gameOverText "GAME OVER!", continueText "Press [f1] to Continue | press [enter] to save score", scoreText, livesText]
    EnteringName -> pictures [gameOverText "ENTER YOUR NAME:", nameText, continueText "Press [ENTER] to save"] -- TODO Consertar essa tela
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
        enterNameText msg = Color white $ Translate (-50) 5 $ Scale 0.1 0.1 $ Text msg
        nameText = Color white $ Translate (-200) (-10) $ Scale 0.1 0.1 $ Text (_playerName g) --TODO consertar isso

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
