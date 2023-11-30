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




initialState :: Game
initialState = b2

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (playerAttr, fg V.cyan)
    , (boxAttr, fg V.red)  -- For boxes not on a target
    , (boxOnTargetAttr, fg V.green)  -- For boxes on a target
    , (wallAttr, fg V.black)
    , (targetAttr, fg V.blue)
    ]



movePlayer :: So.Direction -> EventM () Game ()
movePlayer direction  = do
    gs <- get
    let gs' = step direction gs
    put gs'



handleEvent:: BrickEvent () e -> EventM () Game ()
handleEvent (VtyEvent (EvKey key [])) =
    case key of
        KChar 'w' -> movePlayer up
        KChar 's' -> movePlayer down 
        KChar 'a' -> movePlayer left 
        KChar 'd' -> movePlayer right
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
                $ str $ "Score: " ++ score ++ "/" ++ total
                where
                    score = show (getScore g)
                    total = show (getNumTarget g)

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

drawGame :: Game -> Widget ()
drawGame gs = center $ border $ vBox rows
    where
        rows = [hBox $ cellsInRow y | y <- [0..boardSize-1]]
        cellsInRow y = [cell (V2 x y) | x <- [0..boardSize-1]]
        boxPositions = toList (getBoxes gs)
        targetPositions = toList (getTargets gs)
        boxesOnTargets = [pos | (pos, onTarget) <- zip boxPositions (toList (So.checkOnTarget (getBoxes gs) (getTargets gs))), onTarget]
        cell pos
            | pos == getUser gs = withAttr playerAttr $ str " P "
            | pos `elem` boxesOnTargets = withAttr boxOnTargetAttr $ str " B "  -- Green for boxes on a target
            | pos `elem` boxPositions = withAttr boxAttr $ str " B "  -- Red for boxes not on a target
            | pos `elem` toList (getWall gs) = withAttr wallAttr $ str " W "
            | pos `elem` targetPositions = withAttr targetAttr $ str " T "
            | otherwise = str "   "




-- Attributes for the player and the box
playerAttr, boxAttr :: AttrName
playerAttr = attrName "playerAttr"
boxAttr = attrName "boxAttr"
wallAttr = attrName "wallAttr"
targetAttr = attrName "targetAttr"
boxOnTargetAttr = attrName "boxOnTargetAttr"


box = hBox $ replicate 3 (str " ")
user = str "P"