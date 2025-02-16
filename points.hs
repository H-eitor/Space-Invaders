module Point where
type Point a = (a, a)

pSum :: Num a => Point a -> Point a -> Point a
pSum (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

pMinus :: Num a => Point a -> Point a -> Point a
pMinus (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)

pMult :: Num a => a -> Point a -> Point a
pMult k (x, y) = (k * x, k * y)

pNegate :: Num a => Point a -> Point a
pNegate (x, y) = (negate x, negate y)