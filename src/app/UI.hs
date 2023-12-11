module UI where

import Sokoban as So
import Linear.V2 (V2(..))
import Data.Foldable (toList)
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center
import Brick.BChan(BChan, writeBChan)
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

import qualified Data.Map as M hiding (update)
import qualified Data.Sequence as S
import Data.Sequence (Seq)
import Data.Bool (Bool)
import GHC.Generics (S)
import Data.String (String)


-- Some type definitions and helper data
-- Attributes for the player and the box
playerAttr, boxAttr,wallAttr,targetAttr,boxOnTargetAttr, holeAttr, fragileAttr, iceAttr :: AttrName
playerAttr = attrName "playerAttr"
boxAttr = attrName "boxAttr"
wallAttr = attrName "wallAttr"
targetAttr = attrName "targetAttr"
boxOnTargetAttr = attrName "boxOnTargetAttr"
holeAttr = attrName "holeAttr"
fragileAttr = attrName "fragileAttr"
iceAttr = attrName "iceAttr"

doorAttr, switchAttr, railAttr :: AttrName
doorAttr = attrName "doorAttr"
switchAttr = attrName "switchAttr"
railAttr = attrName "railAttr"

redBoxAttr, blueBoxAttr, redTargetAttr, blueTargetAttr :: AttrName
redBoxAttr = attrName "redBoxAttr"
blueBoxAttr = attrName "blueBoxAttr"
wildBoxAttr = attrName "wildBoxAttr"
redTargetAttr = attrName "redTargetAttr"
blueTargetAttr = attrName "blueTargetAttr"

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
userFigure = " ♀️ "

boxFigure :: String
boxFigure = " ▣ "

targetFigure :: String
targetFigure = " ⚑ "

wallFigure :: String
wallFigure = " 田"
holeFigure :: String
holeFigure = " ○ "

fragileFigure :: String
fragileFigure = " ⚠️ "

iceFigure :: String
iceFigure = " ❄️ " 

doorFigure :: String
doorFigure = " █ "

switchFigure :: String
switchFigure = " ● "

railFigure :: String
railFigure = " # "

initialState :: Game
initialState = loadMap 2

allMaps :: [Game]
allMaps = [b1, b2, b3]

mapNames :: [String]
mapNames = ["Map 1","Map 2","Map 3"]

-- App: Entry of UI
app :: App Game TimerEvent ()
app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = pure ()
          , appAttrMap = const theMap   -- Empty for now
          }


-- -- App required functions
drawUI :: Game -> [Widget ()]
drawUI g = case getMenuStatus g of
            MainMenu -> drawMainMenu g
            MapSelection ->  drawMapSelection g
            GamePlay ->  drawGamePlay g


drawGamePlay :: Game -> [Widget ()]
drawGamePlay g = [center
                    $ withBorderStyle BS.unicode
                    $ borderWithLabel (str " Sokoban Game ")
                    $ hLimit 80 $ vLimit 30
                    $ hBox [padRight (Pad 2) (drawScore g), drawGame g, padLeft (Pad 2) drawHelp]]


drawMainMenu :: Game -> [Widget n]
drawMainMenu gs = [ vBox [ drawTitle
                              , padTop (Pad 1) $ drawGameMode $ getGameMode gs
                              ]]

drawTitle :: Widget n
drawTitle = withAttr titleAttr $ center title

drawGameMode :: GameMode -> Widget n
drawGameMode gm = center $ vBox $ map (uncurry drawModeOption) options
  where
    options = [(Single, "Single Player"), (Multi, "Multiplayer")]
    isSelected Single = gm == Single
    isSelected Multi = gm == Multi
    drawModeOption mode = selectable (isSelected mode)

selectable :: Bool -> String -> Widget n
selectable True  w = withAttr selectedAttr . hCenter $ str $ " >> " ++ w
selectable False w = withAttr normalAttr . hCenter $ str $ "    " ++ w


drawMapSelection :: Game -> [Widget ()]
drawMapSelection g = [center $ vBox (titleWidget : mapSelectionWidgets)]
  where
    titleWidget = withAttr titleAttr $ str "Select Map"
    mapSelectionWidgets = zipWith (drawMapOption (getMapIdx g)) [0..] mapNames

    drawMapOption :: Int -> Int -> String -> Widget ()
    drawMapOption currentIndex idx mapName =
        if currentIndex == idx
        then withAttr selectedAttr $ str $ " >> " ++ mapName
        else withAttr normalAttr $ str $ "    " ++ mapName



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
    | isGameFailed gs = drawFail
    | isGameSuccessful gs = drawSuccess
    | otherwise = center $ border $ vBox rows
  where
    rows = [hBox $ cellsInRow y | y <- [0..boardSize-1]]
    cellsInRow y = [cell (V2 x y) | x <- [0..boardSize-1]]
    boxPositions = toList (getBoxes gs)
    boxesOnTargets = [pos | (pos, onTarget) <- zip boxPositions (toList (So.checkOnTarget (getBoxes gs) (getTargets gs))), onTarget]
    holePositions = toList (getHoles gs)
    fragilePositions = toList (getFragiles gs)
    icePositions = toList (getIces gs)
    redBoxPositions = toList $ getColoredBoxPositions "red" gs
    blueBoxPositions = toList $ getColoredBoxPositions "blue" gs
    redTargetPositions = toList $ getColoredTargetPositions "red" gs
    blueTargetPositions = toList $ getColoredTargetPositions "blue" gs
    doorPositions = toList (getDoor gs)
    railPositions = toList (getRail gs) ++ toList (getRailEnEx gs)

    cell pos
        | pos == getUser gs = withAttr playerAttr $ str userFigure
        | pos `elem` boxesOnTargets = withAttr boxOnTargetAttr $ str boxFigure  -- Green for boxes on a target
        | pos `elem` toList (getWall gs) = withAttr wallAttr $ str wallFigure
        | pos `elem` redBoxPositions = withAttr redBoxAttr $ str boxFigure
        | pos `elem` blueBoxPositions = withAttr blueBoxAttr $ str boxFigure
        | pos `elem` redTargetPositions = withAttr redTargetAttr $ str targetFigure
        | pos `elem` blueTargetPositions = withAttr blueTargetAttr $ str targetFigure
        | pos `elem` boxPositions && not (pos `elem` redBoxPositions || pos `elem` blueBoxPositions)  = withAttr wildBoxAttr $ str boxFigure
        | pos `elem` holePositions = withAttr holeAttr $ str holeFigure
        | pos `elem` icePositions = withAttr iceAttr $ str iceFigure
        | pos `elem` fragilePositions = withAttr fragileAttr $ str fragileFigure
        | pos `elem` doorPositions = withAttr doorAttr $ str doorFigure
        | pos `elem` railPositions = withAttr railAttr $ str railFigure
        | pos == getSwitch gs = withAttr switchAttr $ str switchFigure
        | otherwise = str $ replicate 3 ' '

getColoredBoxPositions :: String -> Game -> Seq Coord
getColoredBoxPositions color game =
    case M.lookup color (getBoxIdx game) of
        Just indices -> indices2Seq indices (getBoxes game)
        Nothing -> S.empty


getColoredTargetPositions :: String -> Game -> Seq Coord
getColoredTargetPositions color game =
    case M.lookup color (getBoxIdx game) of
        Just indices -> indices2Seq indices (getTargets game)
        Nothing -> S.empty

indices2Seq :: Seq Int -> Seq Coord -> Seq Coord
indices2Seq indices coords = S.fromList $ map (S.index coords) (toList indices)


isGameSuccessful :: Game -> Bool
isGameSuccessful = checkSuccess

isGameFailed :: Game -> Bool
isGameFailed = getDead

drawSuccess :: Widget ()
drawSuccess =
    center $
    vBox [str "Success!", str "You solved the puzzle!", str "Press 'R' to re-start.", str "Press 'Q' to quit."]

drawFail :: Widget ()
drawFail =
    center $
    vBox [str "Failed!", str "The game is dead!", str "Press 'R' to re-start.", str "Press 'Q' to quit."]


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (playerAttr, fg V.cyan)
    , (boxOnTargetAttr, fg V.green)
    , (wallAttr, fg V.black)
    , (titleAttr, fg V.cyan `V.withStyle` V.bold `V.withStyle` V.italic)
    , (selectedAttr, fg V.green `V.withStyle` V.bold)
    , (normalAttr, fg V.white)
    , (holeAttr, fg V.black)
    , (iceAttr, fg V.white)
    , (fragileAttr, fg V.magenta)
    , (redBoxAttr, fg V.red)
    , (blueBoxAttr, fg V.blue)
    , (wildBoxAttr, fg V.yellow)
    , (redTargetAttr, fg V.red)
    , (blueTargetAttr, fg V.blue)
    , (doorAttr, fg V.green)
    , (switchAttr, fg V.red)
    , (railAttr, fg V.black)
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
    if getMenuStatus gs == MainMenu
    then case key of
        KChar 'w' -> put $ updateGameMode gs Single
        KChar 's' -> put $ updateGameMode gs Multi
        KEnter    -> put $ updateMenuStatus gs MapSelection
        KChar 'q' -> halt
        _         -> return ()
    else if getMenuStatus gs == MapSelection
    then case key of
        KChar 'w' -> moveSelection Up gs
        KChar 's' -> moveSelection Down gs
        KEnter     -> put $ startTimer $ updateMenuStatus (loadMap (getMapIdx gs)) GamePlay
        KChar 'q' -> halt
        _         -> return ()
    else if isGameSuccessful gs || isGameFailed gs
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


moveSelection :: Brick.Direction -> Game -> EventM () Game ()
moveSelection dir gs = do
    let idx = getMapIdx gs
    let newIdx = if dir == Up then max 0 (idx - 1) else min (length mapNames - 1) (idx + 1)
    put $ updateMapIdx gs newIdx


loadMap :: Int -> Game
loadMap idx = allMaps !! idx


movePlayer :: So.Direction -> EventM () Game ()
movePlayer direction  = do
    gs <- get
    let gs' = step direction gs
    if isGameSuccessful gs'
    then put $ haltTimer gs'
    else put gs'

restartGame :: EventM () Game ()
restartGame = do
    gs <- get
    let currentMapIdx = getMapIdx gs
    let currentGameMode = getGameMode gs
    let currentUIState = getMenuStatus gs

    let resetGameState = loadMap currentMapIdx
    let updatedGameModeState = updateGameMode resetGameState currentGameMode
    let updatedUIState = updateMenuStatus updatedGameModeState currentUIState
    let finalState = startTimer updatedUIState

    put finalState



-- Constants for the game board size and positions
boardSize :: Int
boardSize = 10  -- Change this value to your desired board size


asciiS, asciiO, asciiK, asciiB, asciiA, asciiN :: String
asciiS = " SSSS \nS     \n SSS  \n    S \nSSSS  "
asciiO = " OOO  \nO   O \nO   O \nO   O \n OOO  "
asciiK = "K   K \nK  K  \nKKK   \nK  K  \nK   K "
asciiB = "BBBB  \nB   B \nBBBB  \nB   B \nBBBB  "
asciiA = "  AAA   \n A   A  \n AAAAA  \nA     A \nA     A "
asciiN = "N   N \nNN  N \nN N N \nN  NN \nN   N "

drawAscii :: String -> Widget n
drawAscii = str

title :: Widget n
title = hBox [ padRight (Pad 2) $ drawAscii asciiS,
               padRight (Pad 2) $ drawAscii asciiO,
               padRight (Pad 2) $ drawAscii asciiK,
               padRight (Pad 2) $ drawAscii asciiO,
               padRight (Pad 2) $ drawAscii asciiB,
               padRight (Pad 2) $ drawAscii asciiA,
               padRight (Pad 2) $ drawAscii asciiN ]



