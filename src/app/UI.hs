module UI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.AttrMap
import Brick.Util
import Brick.Widgets.Core
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  , customMain, neverShowCursor,halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )

import Graphics.Vty.Input (Key(..), Event(..))
import qualified Graphics.Vty as V
import Control.Monad.State

data GameState = GameState{
    playerPos :: (Int, Int),
    boxPos :: (Int, Int)
}


initialState :: GameState
initialState = GameState{
    playerPos = (3, 3),
    boxPos = (5, 5)
}

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (playerAttr, fg V.green)
    , (boxAttr, fg V.red)
    ]

movePlayer :: (Int, Int) -> EventM () GameState ()
movePlayer (dx, dy)  = do
    gs <- get
    let (x, y) = playerPos gs
    let newPos = (x + dx, y + dy)
    put $ gs {playerPos = newPos}

handleEvent:: BrickEvent () e -> EventM () GameState ()
handleEvent (VtyEvent (EvKey key [])) =
    case key of
        KChar 'w' -> movePlayer (0, -1)
        KChar 's' -> movePlayer (0, 1) 
        KChar 'a' -> movePlayer (-1,0) 
        KChar 'd' -> movePlayer (1, 0)
        KChar 'q' -> halt 
        _         -> return ()
handleEvent _ = return ()


app :: App GameState e ()
app = App { appDraw = drawGame
          , appChooseCursor = showFirstCursor 
          , appHandleEvent = handleEvent  
          , appStartEvent = pure ()
          , appAttrMap = const theMap   -- Empty for now
          }

-- Constants for the game board size and positions
boardSize :: Int
boardSize = 10  -- Change this value to your desired board size

-- Function to draw the game UI
drawGame :: GameState -> [Widget ()]
drawGame gs = [center $ border $ vBox rows]
    where
        rows = [hBox $ cellsInRow y | y <- [0..boardSize-1]]
        cellsInRow y = [cell (x, y) | x <- [0..boardSize-1]]
        cell pos
            | pos == playerPos gs = withAttr playerAttr $ str " P "
            | pos == boxPos gs = withAttr boxAttr $ str " B "
            | otherwise = str "   "


-- Attributes for the player and the box
playerAttr, boxAttr :: AttrName
playerAttr = attrName "playerAttr"
boxAttr = attrName "boxAttr"


-- Box User, HandleEvent
-- Stats, Wall



box = hBox $ replicate 3 (str " ")
user = str "P"