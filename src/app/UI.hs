module UI where

import Sokoban as So
import Linear.V2 (V2(..))
import Data.Foldable (toList)
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style as BS
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

-- data Game = Game{
--     playerPos :: (Int, Int),
--     boxPos :: [(Int, Int)],
--     wallPos :: [(Int, Int)],
--     targetPos :: [(Int, Int)],
--     score :: Int
-- }


-- initialState :: Game
-- initialState = Game{
--     playerPos = (3, 3),
--     boxPos = [(5, 5), (4, 4), (6, 6)],
--     wallPos = [ (x, 0) | x <- [0..9]] ++ 
--               [ (x, 9) | x <- [0..9]] ++  
--               [ (0, y) | y <- [1..8]] ++ 
--               [ (9, y) | y <- [1..8]],     
--     targetPos = [(6, 7), (2, 3), (4, 3)],
--     score = 100
-- }

initialState :: Game
initialState = b2

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (playerAttr, fg V.green)
    , (boxAttr, fg V.red)
    , (wallAttr, fg V.black)
    , (targetAttr, fg V.blue)
    ]

-- movePlayer :: (Int, Int) -> EventM () Game ()
-- movePlayer (dx, dy)  = do
--     gs <- get
--     let (x, y) = playerPos gs
--     let newPos = (x + dx, y + dy)
--     put $ gs {playerPos = newPos}

movePlayer :: So.Direction -> EventM () Game ()
movePlayer direction  = do
    gs <- get
    let gs = step direction gs
    put gs

handleEvent:: BrickEvent () e -> EventM () Game ()
handleEvent (VtyEvent (EvKey key [])) =
    case key of
        KChar 'w' -> movePlayer up
        KChar 's' -> movePlayer down 
        KChar 'a' -> movePlayer left 
        KChar 'd' -> movePlayer right
        KUp       -> movePlayer up
        KDown     -> movePlayer down
        KLeft     -> movePlayer left
        KRight    -> movePlayer right
        KChar 'q' -> halt 
        _         -> return ()
handleEvent _ = return ()


app :: App Game e ()
app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor 
          , appHandleEvent = handleEvent  
          , appStartEvent = pure ()
          , appAttrMap = const theMap   -- Empty for now
          }

-- Constants for the game board size and positions
boardSize :: Int
boardSize = 10  -- Change this value to your desired board size

drawUI :: Game -> [Widget ()]
drawUI g = [center $ withBorderStyle BS.unicode
            $ borderWithLabel (str " Sokoban Game ")
            $ hLimit 80 $ vLimit 30
            $ hBox [padRight (Pad 2) (drawScore g), drawGame g, padLeft (Pad 2) drawHelp]]


drawScore :: Game -> Widget ()
drawScore g = withBorderStyle BS.unicode
                $ borderWithLabel (str " Score ")
                $ padAll 1 
                $ hLimit 20 
                $ str $ "Score: " ++ show (100)

drawHelp :: Widget ()
drawHelp = withBorderStyle BS.unicode
            $ borderWithLabel (str " Help ")
            $ padAll 1
            $ vBox [ str "Controls:"
                   , str " W - Move Up"
                   , str " S - Move Down"
                   , str " A - Move Left"
                   , str " D - Move Right"
                   , str " Q - Quit Game"
                   , str "Arrow keys also work"
                   ]

-- Function to draw the game UI
-- drawGame :: Game -> Widget ()
-- drawGame gs = center $ border $ vBox rows
--     where
--         rows = [hBox $ cellsInRow y | y <- [0..boardSize-1]]
--         cellsInRow y = [cell (x, y) | x <- [0..boardSize-1]]
--         cell pos
--             | pos == playerPos gs = withAttr playerAttr $ str " P "
--             | pos `elem` boxPos gs = withAttr boxAttr $ str " B "
--             | pos `elem` wallPos gs = withAttr wallAttr $ str " W "
--             | pos `elem` targetPos gs = withAttr targetAttr $ str " T "
--             | otherwise = str "   "

drawGame :: Game -> Widget ()
drawGame gs = center $ border $ vBox rows
    where
        rows = [hBox $ cellsInRow y | y <- [0..boardSize-1]]
        cellsInRow y = [cell (V2 x y) | x <- [0..boardSize-1]]
        cell pos
            | pos == getUser gs = withAttr playerAttr $ str " P "
            | pos `elem` toList (getBoxes gs) = withAttr boxAttr $ str " B "
            | pos `elem` toList (getWall gs) = withAttr wallAttr $ str " W "
            | pos `elem` toList (getTargets gs) = withAttr targetAttr $ str " T "
            | otherwise = str "   "


-- Attributes for the player and the box
playerAttr, boxAttr :: AttrName
playerAttr = attrName "playerAttr"
boxAttr = attrName "boxAttr"
wallAttr = attrName "wallAttr"
targetAttr = attrName "targetAttr"

-- Box User, HandleEvent
-- Stats, Wall


box = hBox $ replicate 3 (str " ")
user = str "P"