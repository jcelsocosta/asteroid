module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM

data GameAttribute =  Score  Int

numbers = 0
width = 400
height = 400
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

texbmp = [("tex.bmp",Nothing)]

main :: IO ()
main = do
  
  let winConfig = ((100,80),(width,height),"Pong")
      bmpList = texbmp 
        --[(texbmp, Nothing)]
      gameMap = textureMap 0 30 30 w h
      bar     = objectGroup "barGroup"  [createBar]
      ball    = objectGroup "ballGroup" [createBall,createBall2,createBall3,createBall4,createBall5]
      initScore = Score 0
      input = [
        (SpecialKey KeyRight, StillDown, moveBarToRight)
        ,(SpecialKey KeyLeft,  StillDown, moveBarToLeft)
        ,(SpecialKey KeyUp,  StillDown, moveBarToUp)
        ,(SpecialKey KeyDown,  StillDown, moveBarToDown)
        ,(Char 'q',            Press,     \_ _ -> funExit)
        ]

  funInit winConfig gameMap [bar,ball] () initScore input gameCycle (Timer 30) bmpList



createBall ::  GameObject ()
createBall =
  let ballPic = Basic (Circle 17.0 0.0 1.0 0.0 Filled)
  in object "ball" ballPic False (w/2,h-10) (1,-5) ()

createBall2 :: GameObject ()
createBall2 =
  let ballPic = Basic (Circle 17.0 0.0 1.0 0.0 Filled)
  in object "ball2" ballPic False (w-50,10000) (0,-8) ()

createBall3 :: GameObject ()
createBall3 =
  let ballPic = Basic (Circle 17.0 0.0 1.0 0.0 Filled)
  in object "ball3" ballPic False ((w/2)+60,20000) (0,-6) ()

createBall4 :: GameObject ()
createBall4 =
  let ballPic = Basic (Circle 17.0 0.0 1.0 0.0 Filled)
  in object "ball4" ballPic False (50,30000) (0,-7) ()

createBall5 :: GameObject ()
createBall5 =
  let ballPic = Basic (Circle 17.0 0.0 1.0 0.0 Filled)
  in object "ball5" ballPic False ((w/2)-50,40000) (0,-6) ()

createBar :: GameObject ()
createBar =
  let barBound = [(-25,-6),(25,-6),(25,6),(-25,6)]
      barPic   = Basic (Polyg barBound 2.0 2.0 2.0 Filled)
  in object "bar" barPic False (w/2,30) (0,0) ()



moveBarToRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBarToRight _ _ = do
  obj     <- findObject "bar" "barGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 5 <= w)
   then (setObjectPosition ((pX + 5),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

moveBarToLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBarToLeft _ _ = do
  obj <- findObject "bar" "barGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 5 >= 0)
    then (setObjectPosition ((pX - 5),pY) obj)
    else (setObjectPosition (sX/2,pY) obj)

moveBarToUp :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBarToUp _ _ = do
  obj <- findObject "bar" "barGroup"
  (pX,pY) <- getObjectPosition obj
  (_,sY) <- getObjectSize obj
  if (pY - (sY/2)+5 <= h)
    then (setObjectPosition (pX,(pY+5)) obj)
    else (setObjectPosition (pX,(h-(sY/2))) obj)

moveBarToDown :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBarToDown _ _ = do
  obj <- findObject "bar" "barGroup"
  (pX,pY) <- getObjectPosition obj
  (_,sY) <- getObjectSize obj
  if (pY - (sY/2)-5 >= 0)
    then (setObjectPosition (pX,(pY-5)) obj)
    else (setObjectPosition (pX,(sY/2)) obj)

gameCycle :: IOGame GameAttribute () () () ()
gameCycle = do
  (Score n) <- getGameAttribute
  
  
  
  printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0
 

  ball <- findObject "ball" "ballGroup"
  ball2 <- findObject "ball2" "ballGroup"
  ball3 <- findObject "ball3" "ballGroup"
  ball4 <- findObject "ball4" "ballGroup"
  ball5 <- findObject "ball5" "ballGroup"
  bar <- findObject "bar" "barGroup"
  
  col4 <- objectBottomMapCollision ball
  when col4  ( do
    setObjectPosition ((w/2),h-20)   ball
    setGameAttribute (Score (n+1))
       
              )

  when (n<3)(do
      setObjectPosition ((w-50),h+100) ball2
              )
    
 
  when (n<8) (do
      setObjectPosition ((w/2)+50,h+100) ball3
            )
    
  when (n<10)(do
      setObjectPosition (50,h+100) ball4
              )
    
  when (n<12)(do
      setObjectPosition ((w/2)-50,h+100) ball5
             )   

  col6 <- objectBottomMapCollision ball2
  when col6 ( do
      setObjectPosition ((w-50),h-10) ball2
      setGameAttribute (Score (n+1))

              )
  col8 <- objectBottomMapCollision ball3
  when col8 (do
      setObjectPosition ((w/2)+50,h) ball3
      setGameAttribute (Score (n+1))
            )

  col10 <- objectBottomMapCollision ball4
  when col10 (do
      setObjectPosition (50,h-50) ball4
      setGameAttribute (Score (n+1))
              )
  col12 <- objectBottomMapCollision ball5
  when col12 (do
      setObjectPosition ((w/2)-50,h-50) ball5
      setGameAttribute (Score (n+1))
              )

  col5 <- objectsCollision ball bar
  col7 <- objectsCollision ball2 bar
  col9 <- objectsCollision ball3 bar
  col11 <- objectsCollision ball4 bar
  col13 <- objectsCollision ball5 bar
  when (col5 || col7 || col9 || col11 || col13)  (do
              funExit
            )