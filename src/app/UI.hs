module UI where

import Sokoban as So
import Linear.V2 (V2(..))
import Data.Foldable (toList)
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center
import Brick.BChan(BChan, writeBChan)
import Brick.Widgets.Core ((<+>), (<=>), vBox, hBox)
import Brick.Types()
import Brick (
    App(..), AttrMap, BrickEvent(..), EventM, Widget
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
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Text.Printf (printf)
import GHC.Conc.Sync (ThreadId)

-- Some type definitions and helper data
-- Attributes for the player and the box
playerAttr, boxAttr,wallAttr,targetAttr,boxOnTargetAttr :: AttrName
playerAttr = attrName "playerAttr"
boxAttr = attrName "boxAttr"
wallAttr = attrName "wallAttr"
targetAttr = attrName "targetAttr"
boxOnTargetAttr = attrName "boxOnTargetAttr"

titleAttr, selectedAttr, normalAttr :: AttrName
titleAttr = attrName "title"
selectedAttr = attrName "selected"
normalAttr = attrName "normal"

-- Timer related definitions
data TimerEvent = Tick
startTicking :: Int -> BChan TimerEvent -> IO GHC.Conc.Sync.ThreadId
startTicking delay channel = forkIO $ forever $ do
    threadDelay delay -- delay is in microseconds
    writeBChan channel Tick

-- Unicode related definitions
userFigure :: String
userFigure = " 🏃"

boxFigure :: String
boxFigure = " ▣ "

targetFigure :: String
targetFigure = " ⚑ "

wallFigure :: String
wallFigure = " 🧱"

wallSpaceCost :: Int
wallSpaceCost = 2

initialState :: Game
initialState = b2




-- App: Entry of UI
app :: App Game TimerEvent ()
app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor 
          , appHandleEvent = handleEvent  
          , appStartEvent = pure ()
          , appAttrMap = const theMap   -- Empty for now
          }

-- createBorderOverlay :: Int -> Int -> Widget ()
-- createBorderOverlay w h = 
--     withBorderStyle unicodeBold $ 
--     center $ 
--     hLimit h $ 
--     vLimit w $ 
--     border $ 
--     str " dsafaf " 


createCustomBorder :: Widget n -> Widget n
createCustomBorder widgetInside = 
    withBorderStyle unicodeRounded $  
    borderWithLabel (str "Custom Border") $ 
    widgetInside

-- Function to create the main game UI
drawGameUI :: Game -> Widget ()
drawGameUI g = center 
                    $ hLimit 80 $ vLimit 30
                    $ hBox [padRight (Pad 2) (drawScore g), drawGame g]
                
-- drawUI :: Game -> [Widget ()]
-- drawUI g = 
--     if getMenuStatus g
--     then drawMainMenu g
--     else let gameUI = drawGameUI g
--              customBorderedGameUI = createCustomBorder gameUI
--          in [customBorderedGameUI]


-- -- App required functions
drawUI :: Game -> [Widget ()]
drawUI g = if getMenuStatus g
           then drawMainMenu g
           else [center 
                    $ hLimit 80 $ vLimit 30
                    $ hBox [padRight (Pad 2) (drawScore g), drawGame g]]

drawMainMenu :: Game -> [Widget n]
drawMainMenu gs = [ vBox [ drawTitle
                              , padTop (Pad 2) $ drawGameMode $ getGameMode gs
                              ]]

drawTitle :: Widget n
drawTitle = withAttr titleAttr $ center $ str "Sokoban Game"

drawGameMode :: GameMode -> Widget n
drawGameMode gm = center $ vBox $ map (uncurry drawModeOption) options
  where
    options = [(Single, "Single Player"), (Multi, "Multiplayer")]
    isSelected Single = gm == Single
    isSelected Multi = gm == Multi
    drawModeOption mode label = selectable (isSelected mode) $ str label

selectable :: Bool -> Widget n -> Widget n
selectable True  = withAttr selectedAttr . hCenter
selectable False = withAttr normalAttr . hCenter


drawScore :: Game -> Widget ()
drawScore g = withBorderStyle BS.unicode
                $ border
                $ padAll 2
                $ hLimit 20 
                $ vBox [ str "Score: " <+> str score <+> str "/" <+> str total
                       , str "Steps: " <+> str steps
                       , str $ formatTime $ getTimer g
                       ]
                where
                    score = show (getScore g)
                    total = show (getNumTarget g)
                    steps = show (getSteps g)

formatTime :: Int -> String
formatTime totalSeconds = printf "%02d:%02d" minutes seconds
  where
    (minutes, seconds) = totalSeconds `divMod` 60

drawHelp :: Widget ()
drawHelp = withBorderStyle BS.unicode
            $ borderWithLabel (str " Help ")
            $ padAll 2
            $ vBox [ str "Controls:"
                   , str " W - Move Up"
                   , str " S - Move Down"
                   , str " A - Move Left"
                   , str " D - Move Right"
                   , str " Q - Quit Game"
                   , str " R - Restart Game"
                   , str "Arrow keys also work"
                   ]

drawGame :: Game -> Widget ()
drawGame gs
    | isGameSuccessful gs = drawSuccess 
    | otherwise = center  $ vBox rows
  where
    rows = [hBox $ cellsInRow y | y <- [0..boardSize-1]]
    cellsInRow y = [cell (V2 x y) | x <- [0..boardSize-1]]
    boxPositions = toList (getBoxes gs)
    targetPositions = toList (getTargets gs)
    boxesOnTargets = [pos | (pos, onTarget) <- zip boxPositions (toList (So.checkOnTarget (getBoxes gs) (getTargets gs))), onTarget]
    cell pos
        | pos == getUser gs = withAttr playerAttr $ str userFigure
        | pos `elem` boxesOnTargets = withAttr boxOnTargetAttr $ str boxFigure  -- Green for boxes on a target
        | pos `elem` boxPositions = withAttr boxAttr $ str boxFigure  -- Red for boxes not on a target
        | pos `elem` toList (getWall gs) = withAttr wallAttr $ str wallFigure
        | pos `elem` targetPositions = withAttr targetAttr $ str targetFigure
        | otherwise = str $ replicate 3 ' '

isGameSuccessful :: Game -> Bool
isGameSuccessful gs =
    getScore gs == getNumTarget gs

drawSuccess :: Widget ()
drawSuccess =
    center $
    vBox [str "Success!", str "You solved the puzzle!", str "Press 'R' to re-start.", str "Press 'Q' to quit."]


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (playerAttr, fg V.cyan)
    , (boxAttr, fg V.red)  -- For boxes not on a target
    , (boxOnTargetAttr, fg V.green)  -- For boxes on a target
    , (wallAttr, fg V.black)
    , (targetAttr, fg V.blue)
    , (titleAttr, fg V.cyan `V.withStyle` V.bold)
    , (selectedAttr, fg V.green `V.withStyle` V.bold)
    , (normalAttr, fg V.white)
    ]

handleEvent :: BrickEvent () TimerEvent -> EventM () Game ()
-- Handle Timer Events
handleEvent (AppEvent Tick) = do
    gs <- get
    let gs' = updateTimer gs
    put gs'

-- Handle Key press Events
handleEvent (VtyEvent (EvKey key [])) = do
    gs <- get
    if getMenuStatus gs
    then case key of
        KChar 'w' -> put $ updateGameMode gs Single
        KChar 's' -> put $ updateGameMode gs Multi
        KEnter    -> put $ startTimer $ updateMenuStatus gs False
        _         -> return ()
    else if isGameSuccessful gs
        then case key of
            KChar 'r' -> restartGame
            KChar 'q' -> halt
            _         -> return ()
        else case key of
            KChar 'w' -> movePlayer up
            KChar 's' -> movePlayer down 
            KChar 'a' -> movePlayer left 
            KChar 'd' -> movePlayer right
            KUp       -> movePlayer up
            KDown     -> movePlayer down
            KLeft     -> movePlayer left
            KRight    -> movePlayer right
            KChar 'r' -> restartGame
            KChar 'q' -> halt
            _         -> return ()
handleEvent _ = return ()

movePlayer :: So.Direction -> EventM () Game ()
movePlayer direction  = do
    gs <- get
    let gs' = step direction gs
    if isGameSuccessful gs' 
    then put $ haltTimer gs'
    else put gs'

restartGame :: EventM () Game ()
restartGame = put initialState


-- Constants for the game board size and positions
boardSize :: Int
boardSize = 10  -- Change this value to your desired board size








