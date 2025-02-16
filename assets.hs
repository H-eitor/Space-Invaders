module Assets where
    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    
    drawPlayer :: Float -> Picture
    drawPlayer x = translate x (-250) (color blue (rectangleSolid 50 20))