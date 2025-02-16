import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as M
import System.Random (newStdGen, randoms)


-- TIPOS
type Pair x = (x, x)

data Status = Running | Won | Lost deriving (Eq) -- TODO

data Item x = Item
    { _siz :: Pair x
    , _pos :: Pair x
    , _vel :: Pair x
    , _hp :: x }

data Game x = Game
    { _status :: Status
    , _inputLeft :: Bool
    , _inputRight :: Bool
    , _inputFire :: Bool
    , _rands :: [x]
    , _firetime :: x
    , _player :: Item x
    , _bullets :: [Item x]
    , _shields :: [Item x]
    , _invaders :: [Item x] }


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
    
startGame :: (Fractional a, Ord a) => [a] -> Game a
startGame rands0 = Game Running False False False rands1 0 player [] shields invaders
    where   player = Item (70, 20) (0, -250) (0, 0) 1
            ([mag, dir], rands1) = splitAt 2 rands0
            vx = (150 + 200 * mag) * (if dir < 0.5 then 1 else -1)
            invaders = [Item (70, 20) 
                            (fromIntegral x * 100, fromIntegral y * 50 + 150)
                            (vx, 0)
                            1
                            | x<-[-2..(2::Int)], y<-[0..(2::Int)] ]
            shields = [Item (70, 15)
                            (fromIntegral x * 150, fromIntegral y * 15 - 180 )
                            (0, 0)
                            1
                            | x <- [-2..(2::Int)], y <- [1..(4::Int)]]

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
          v@(vx, _) = _vel $ head myInvaders
          move v0 v1 i = i { _pos = pSum (_pos i) (pMult time v0), _vel = v1 } 
          i2 | vx>0 && x1max>380 = map (move (380-x1max, 0) (pNegate v)) i1
             | vx<0 && x1min<(-380) = map (move (-380-x1min, 0) (pNegate v)) i1
             | otherwise = i1
          g2 = g { _invaders = i2 }
          g3 = invaderShoot g2

autoUpdateItem :: (Num a) => a -> Item a -> Item a
autoUpdateItem t i@(Item _ pos vel _) = i { _pos = pSum pos (pMult t vel)}

    -- Disparos

playerShoot :: (Fractional a, Ord a) => a -> Game a -> Game a
playerShoot time g = if canFire then mFire else mNofire
    where canFire = _inputFire g && _firetime g > 0.9
          (x, y) = _pos (_player g)
          bullet = Item (3, 15) (x, y+20) (0, 200) 1
          mFire = g { _bullets = bullet : _bullets g, _firetime = 0 }
          mNofire = g { _firetime = time + _firetime g }

invaderShoot :: (Fractional a, Ord a) => Game a -> Game a
invaderShoot g = g { _bullets = _bullets g ++ bs, _rands = rands3 }
    where invadersPos = map _pos $ _invaders g
          fInsert pMap (x,y) = M.insertWith min x y pMap
          fighters0 = M.toList $ foldl fInsert M.empty invadersPos
          (rands0, rands1) = splitAt (length fighters0) (_rands g)
          (rands2, rands3) = splitAt (length fighters0) rands1
          difficulty = 0.9 + fromIntegral (length invadersPos) * (0.99 - 0.9) / 15
          fighters1 = [ (p, v) | (p, r, v) <- zip3 fighters0 rands0 rands2, r > difficulty ]
          createBullet ((x, y), v) = Item (3, 9) (x, y-20) (0, -(300-v*200)) 1
          bs = map createBullet fighters1

updateBullets :: (Num a, Ord a) => a -> Game a -> Game a
updateBullets time g = g { _bullets = b2 }
    where b1 = map (autoUpdateItem time) (_bullets g)
          b2 = filter (\b -> snd (_pos b) < 300 && snd (_pos b) > -300) b1

    -- Colisões

updateCollisions :: (Fractional a, Ord a) => Game a -> Game a
updateCollisions g = g1 { _status = st, _bullets = b3, _shields = s1 , _invaders = i1  }
    where (b1, i1) = runCollisions (_bullets g) (_invaders g)
          (b2, p1) = runCollisions b1 [_player g]
          (b3, s1) = runCollisions b2 (_shields g)
          st | null i1 = Won 
             | null p1 = Lost
             | otherwise = Running
          g1 = if st == Running then g 
               else g { _inputLeft = False, _inputRight = False, _inputFire = False }

testCollision :: (Fractional a, Ord a) => Item a -> Item a -> Bool
testCollision (Item as ap _ _) (Item bs bp _ _) =
    ((bx0 < ax0 && ax0 < bx1) || (bx0 < ax1 && ax1 < bx1)) &&
    ((by0 < ay0 && ay0 < by1) || (by0 < ay1 && ay1 < by1))
    where (ax0, ay0) = pMinus ap (pMult 0.5 as) 
          (ax1, ay1) = pSum ap (pMult 0.5 as)
          (bx0, by0) = pMinus bp (pMult 0.5 bs)
          (bx1, by1) = pSum bp (pMult 0.5 bs)

runCollisions :: (Fractional a, Ord a) => [Item a] -> [Item a] -> ([Item a], [Item a])
runCollisions [] is = ([], is)
runCollisions (b:bs) is = (bs1++bs2, is2)
    where is1 = filter (not . testCollision b) is
          bs1 = [b | length is1 == length is]
          (bs2, is2) = runCollisions bs is1

step :: (Fractional a, Ord a) => a -> Game a -> Game a
step time g = if _status g == Running then mRunning else mEnd
    where mRunning = updatePlayer time
                        $ updateInvaders time 
                        $ updateBullets time
                        $ updateCollisions g
          mEnd = if _inputFire g then startGame (_rands g) else g

-- MAIN

displayH :: Game Float -> Picture
displayH g = case _status g of
    Won -> Color white $ Translate (-170) 0 $ Scale 0.5 0.5 $ Text "YOU WIN!"
    Lost -> Color white $ Translate (-200) 0 $ Scale 0.5 0.5 $ Text "GAME OVER!"
    _ -> pictures $ player : bullets ++ shields ++ invaders
    where player = drawItem green (_player g)
          bullets = map (drawItem yellow) (_bullets g)
          shields = map (drawItem blue) (_shields g)
          invaders = map (drawItem red) (_invaders g)

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
eventH _ g = g

idleH :: Float -> Game Float -> Game Float
idleH = step

main :: IO ()
main = do
    myRands <- randoms <$> newStdGen
    let myGame = startGame myRands
    let myWindow = InWindow "haskell-invaders" (800, 600) (0, 0)
    play myWindow black 30 myGame displayH eventH idleH
