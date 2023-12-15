{-# LANGUAGE ImportQualifiedPost #-}

module UI where

import Brick
import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Padding (..),
    Widget,
    attrMap,
    customMain,
    emptyWidget,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    padAll,
    padLeft,
    padRight,
    padTop,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
  )
import Brick.BChan (BChan, writeBChan)
import Brick.Types ()
import Brick.Widgets.Border
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, unless)
import Data.Bool (Bool)
import Data.Foldable (toList)
import Data.Map qualified as M hiding (update)
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.String (String)
import GHC.Conc.Sync (ThreadId)
import GHC.Generics (S)
import Graphics.Vty qualified as V
import Graphics.Vty.Input (Event (..), Key (..))
import Linear.V2 (V2 (..))
import Sokoban as So
import Text.Printf (printf)

-- Some type definitions and helper data
-- Attributes for the player and the box
playerAttr, boxAttr, wallAttr, targetAttr, boxOnTargetAttr, holeAttr, fragileAttr, iceAttr :: AttrName
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
userFigure = " ♀ "

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
doorFigure = " ⌻ "

switchFigure :: String
switchFigure = " ● "

initialState :: AppState
initialState = appState

allMaps :: [Game]
allMaps = [combined, classicBox, mordenBox, wildCardBox, railBox, icefloorBox, fragilefloorBox, doorBox]

mapNames :: [String]
mapNames = ["combined", "classicBox", "mordenBox", "wildCardBox", "railBox", "icefloorBox", "fragilefloorBox", "doorBox"]

-- App: Entry of UI
app :: App AppState TimerEvent ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const theMap
    }

-- -- App required functions
drawUI :: AppState -> [Widget ()]
drawUI a = case getMenuStatus a of
  MainMenu -> drawMainMenu a
  MapSelection -> drawMapSelection a
  GamePlay -> drawGamePlay a

drawGamePlay :: AppState -> [Widget ()]
drawGamePlay a = case getGameMode a of
  Single -> drawSinglePlayer a
  Multi -> drawMultiPlayer a

drawSinglePlayer :: AppState -> [Widget ()]
drawSinglePlayer a =
  [ center $
      hBox
        [ padRight (Pad 2) drawElementHelp,
          withBorderStyle BS.unicode $
            borderWithLabel (str " Sokoban Game ") $
              hLimit 80 $
                vLimit 30 $
                  hBox
                    [ padRight (Pad 2) (drawScore (getGame1 a) (getTimer $ getGame1 a)),
                      drawGame (getGame1 a),
                      padLeft (Pad 2) (drawHelp Single)
                    ]
        ]
  ]

drawMultiPlayer :: AppState -> [Widget ()]
drawMultiPlayer a =
    if isGameOver (getGame1 a) && isGameOver (getGame2 a)
        then [drawMultiModeSuccess a]
        else
  [ center $
      hBox
        [ padRight (Pad 2) drawElementHelp,
          withBorderStyle BS.unicode $
            borderWithLabel (str " Sokoban Game - Multiplayer ") $
              hLimit 180 $
                vLimit 40 $
                  vBox
                    [ hBox
                        [ vBox
                            [ padRight (Pad 2) (drawScore (getGame1 a) (getTimer $ getGame1 a)),
                              drawGame (getGame1 a)
                            ],
                          padLeft (Pad 4) $
                            padRight
                              (Pad 4)
                              ( vBox
                                  [ drawScore (getGame2 a) (getTimer $ getGame2 a),
                                    drawGame (getGame2 a)
                                  ]
                              )
                        ],
                      padTop (Pad 1) (drawHelp Multi)
                    ]
        ]
  ]

drawMainMenu :: AppState -> [Widget n]
drawMainMenu a =
  [ vBox
      [ drawTitle,
        withBorderStyle BS.unicode $
          border $
            padTop (Pad 1) $
              padAll 2 $
                vBox
                  [ str "Use W/S to traverse the menu",
                    str "Press Enter to confirm selection",
                    str "Use Q to quit the game",
                    str "Note: You may need into resize your terminal for multi-player mode ",
                    str "Enjoy!!"
                  ],
        padTop (Pad 1) $ drawGameMode $ getGameMode a
      ]
  ]

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
selectable True w = withAttr selectedAttr . hCenter $ str $ " >> " ++ w
selectable False w = withAttr normalAttr . hCenter $ str $ "    " ++ w

drawMapSelection :: AppState -> [Widget ()]
drawMapSelection a = [center $ vBox (titleWidget : mapSelectionWidgets)]
  where
    titleWidget = withAttr titleAttr $ str "Select Map"
    mapSelectionWidgets = zipWith (drawMapOption (getMapIdx a)) [0 ..] mapNames

    drawMapOption :: Int -> Int -> String -> Widget ()
    drawMapOption currentIndex idx mapName =
      if currentIndex == idx
        then withAttr selectedAttr $ str $ " >> " ++ mapName
        else withAttr normalAttr $ str $ "    " ++ mapName

drawScore :: Game -> Int -> Widget ()
drawScore g t =
  withBorderStyle BS.unicode $
    border $
      padAll 2 $
        hLimit 20 $
          vBox
            [ str "Score: " <+> str score <+> str "/" <+> str total,
              str "Steps: " <+> str steps,
              str $ formatTime t
            ]
  where
    score = show (getScore g)
    total = show (getNumTarget g)
    steps = show (getSteps g)

formatTime :: Int -> String
formatTime totalSeconds = printf "%02d:%02d" minutes seconds
  where
    (minutes, seconds) = totalSeconds `divMod` 60

drawHelp :: GameMode -> Widget ()
drawHelp gameMode =
  withBorderStyle BS.unicode $
    borderWithLabel (str " Help ") $
      padAll 2 $
        vBox controls
  where
    controls = case gameMode of
      Single ->
        [ str "Controls for Player:",
          str " W - Move Up",
          str " S - Move Down",
          str " A - Move Left",
          str " D - Move Right",
          str " Q - Quit Game",
          str " R - Back to main menu"
        ]
      Multi ->
        [ str " Q - Quit Game",
          str " R - Back to main menu",
          str "Controls for Player 1:",
          str " W - Move Up",
          str " S - Move Down",
          str " A - Move Left",
          str " D - Move Right",
          str "Controls for Player 2:",
          str " ↑ - Move Up",
          str " ↓ - Move Down",
          str " ← - Move Left",
          str " → - Move Right"
        ]

drawGame :: Game -> Widget ()
drawGame gs
  | isGameFailed gs = drawFail
  | isGameSuccessful gs = drawSuccess
  | otherwise = center $ border $ vBox rows
  where
    rows = [hBox $ cellsInRow y | y <- [0 .. boardSize - 1]]
    cellsInRow y = [cell (V2 x y) | x <- [0 .. boardSize - 1]]
    boxPositions = toList (getBoxes gs)
    boxesOnTargets = toList (onTargetBox gs)
    holePositions = toList (getHoles gs)
    fragilePositions = toList (getFragiles gs)
    icePositions = toList (getIces gs)
    redBoxPositions = toList $ getColoredBoxPositions "red" gs
    blueBoxPositions = toList $ getColoredBoxPositions "blue" gs
    redTargetPositions = toList $ getColoredTargetPositions "red" gs
    blueTargetPositions = toList $ getColoredTargetPositions "blue" gs
    doorPositions = toList (getDoor gs)
    railPositions = getRail gs

    cell pos
      | pos == getUser gs = withAttr playerAttr $ str userFigure
      | pos `elem` boxesOnTargets = withAttr boxOnTargetAttr $ str boxFigure -- Green for boxes on a target
      | pos `elem` toList (getWall gs) = withAttr wallAttr $ str wallFigure
      | pos `elem` redBoxPositions = withAttr redBoxAttr $ str boxFigure
      | pos `elem` blueBoxPositions = withAttr blueBoxAttr $ str boxFigure
      | pos `elem` boxPositions && not (pos `elem` redBoxPositions || pos `elem` blueBoxPositions) = withAttr wildBoxAttr $ str boxFigure
      | pos `elem` redTargetPositions = withAttr redTargetAttr $ str targetFigure
      | pos `elem` blueTargetPositions = withAttr blueTargetAttr $ str targetFigure
      | pos `elem` holePositions = withAttr holeAttr $ str holeFigure
      | pos `elem` icePositions = withAttr iceAttr $ str iceFigure
      | pos `elem` fragilePositions = withAttr fragileAttr $ str fragileFigure
      | pos `elem` doorPositions = withAttr doorAttr $ str doorFigure
      | pos `elem` railPositions = withAttr railAttr $ str (drawRail pos (getRail gs S.>< getRailEnEx gs))
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

drawRail :: Coord -> Seq Coord -> [Char]
drawRail (V2 x y) rails
  | isCornerTopLeft = " ╔ "
  | isCornerTopRight = " ╗ "
  | isCornerBottomLeft = " ╚ "
  | isCornerBottomRight = " ╝ "
  | isVerticalLine = " ║ "
  | isHorizontalLine = " ═ "
  | otherwise = "   "
  where
    hasPoint dx dy = V2 (x + dx) (y + dy) `elem` railsList
    railsList = toList rails

    isCornerTopLeft = hasPoint 1 0 && hasPoint 0 1 && not (hasPoint (-1) 0 || hasPoint 0 (-1))
    isCornerTopRight = hasPoint (-1) 0 && hasPoint 0 1 && not (hasPoint 1 0 || hasPoint 0 (-1))
    isCornerBottomLeft = hasPoint 1 0 && hasPoint 0 (-1) && not (hasPoint (-1) 0 || hasPoint 0 1)
    isCornerBottomRight = hasPoint (-1) 0 && hasPoint 0 (-1) && not (hasPoint 1 0 || hasPoint 0 1)
    isVerticalLine = hasPoint 0 (-1) && hasPoint 0 1
    isHorizontalLine = hasPoint (-1) 0 && hasPoint 1 0

isGameSuccessful :: Game -> Bool
isGameSuccessful = checkSuccess

isGameFailed :: Game -> Bool
isGameFailed = getDead

drawSuccess :: Widget ()
drawSuccess =
  center $
    vBox [str "Success!", str "You solved the puzzle!", str "Press 'R' to re-start.", str "Press 'Q' to quit."]

drawMultiModeSuccess :: AppState -> Widget ()
drawMultiModeSuccess as=
    let game1 = getGame1 as
        game2 = getGame2 as
        steps1 = getSteps game1
        steps2 = getSteps game2
        time1 = getTimer game1
        time2 = getTimer game2
        no_winner = steps1 == steps2 && time1 == time2
        winner
          | steps1 < steps2 = "Player1"
          | steps1 > steps2 = "Player2"
          | time1 < time2 = "Player1"
          | time1 > time2 = "Player2"
          | otherwise = "Both of you!"
        in
            center $
                vBox [str "Success!", 
                         str ("Player1: " ++ show steps1 ++ " steps, " ++ show time1 ++ " seconds"),
                         str ("Player2: " ++ show steps2 ++ " steps, " ++ show time2 ++ " seconds"),
                         str (if no_winner then "Same performance, well done!" else winner ++ " wins!"),
                         str "Press 'R' to back to main menu.", str "Press 'Q' to quit."]



drawFail :: Widget ()
drawFail =
  center $
    vBox [str "Failed!", str "The game is dead!", str "Press 'R' back to main menu.", str "Press 'Q' to quit."]

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (playerAttr, fg V.cyan),
      (boxOnTargetAttr, fg V.green),
      (wallAttr, fg V.black),
      (titleAttr, fg V.cyan `V.withStyle` V.bold `V.withStyle` V.italic),
      (selectedAttr, fg V.green `V.withStyle` V.bold),
      (normalAttr, fg V.white),
      (holeAttr, fg V.black),
      (iceAttr, fg V.white),
      (fragileAttr, fg V.magenta),
      (redBoxAttr, fg V.red),
      (blueBoxAttr, fg V.blue),
      (wildBoxAttr, fg V.yellow),
      (redTargetAttr, fg V.red),
      (blueTargetAttr, fg V.blue),
      (doorAttr, fg V.green),
      (switchAttr, fg V.red),
      (railAttr, fg V.white)
    ]

-- Helper function to check if the game is over (success or failure)
isGameOver :: Game -> Bool
isGameOver game = isGameSuccessful game || isGameFailed game

handleEvent :: BrickEvent () TimerEvent -> EventM () AppState ()
-- Handle Timer Events
handleEvent (AppEvent Tick) = do
  as <- get
  let gs1 = getGame1 as
  let gs2 = getGame2 as
  let gs1' = updateTimer gs1
  let  gs2' = updateTimer gs2
  let  as' = updateGame1 (updateGame2 as gs2') gs1'
  put as'



-- Handle Key press Events
handleEvent (VtyEvent (EvKey key [])) = do
  as <- get

  let game1Over = isGameOver (getGame1 as)
  let game2Over = isGameOver (getGame2 as)

  case getMenuStatus as of
    MainMenu -> case key of
      KChar 'w' -> put $ updateGameMode as Single
      KChar 's' -> put $ updateGameMode as Multi
      KEnter -> put $ updateMenuStatus as MapSelection
      KChar 'q' -> halt
      _ -> return ()
    MapSelection -> case key of
      KChar 'w' -> moveSelection Up as
      KChar 's' -> moveSelection Down as
      KEnter -> put $ startTimer $ updateMenuStatus (updateGame2 (updateGame1 as (loadMap (getMapIdx as))) (loadMap (getMapIdx as))) GamePlay
      KChar 'q' -> halt
      _ -> return ()
    GamePlay -> case getGameMode as of
      Single -> unless game1Over $ handleSinglePlayerKeys key
      Multi -> handleMultiPlayerKeys key game1Over game2Over

  -- Common keys for all modes
  case key of
    KChar 'r' -> restartGame
    KChar 'q' -> halt
    _ -> return ()
  where
    handleSinglePlayerKeys k = case k of
      KChar 'w' -> movePlayer1 up
      KChar 's' -> movePlayer1 down
      KChar 'a' -> movePlayer1 left
      KChar 'd' -> movePlayer1 right
      _ -> return ()

    handleMultiPlayerKeys k g1Over g2Over = case k of
      KChar 'w' -> unless g1Over $ movePlayer1 up
      KChar 's' -> unless g1Over $ movePlayer1 down
      KChar 'a' -> unless g1Over $ movePlayer1 left
      KChar 'd' -> unless g1Over $ movePlayer1 right
      KUp -> unless g2Over $ movePlayer2 up
      KDown -> unless g2Over $ movePlayer2 down
      KLeft -> unless g2Over $ movePlayer2 left
      KRight -> unless g2Over $ movePlayer2 right
      _ -> return ()
handleEvent _ = return ()

drawElementHelp :: Widget ()
drawElementHelp =
  withBorderStyle BS.unicode $
    borderWithLabel (str " Element Help ") $
      padAll 2 $
        vBox
          [ withAttr playerAttr (str userFigure) <+> str "  Player ",
            str "You can move it with W/A/S/D",
            withAttr redBoxAttr (str boxFigure) <+> str "  Box ",
            str "You need to push the box to a target with the same color",
            withAttr boxOnTargetAttr (str boxFigure) <+> str "  Success Box " ,
            str "You have successfully pushed it to a target",
            withAttr wildBoxAttr (str boxFigure) <+> str "  Wild Box " ,
            str "It may help you to solve the puzzle but not counting to score",
            withAttr redTargetAttr (str targetFigure) <+> str "  Target ",
            str "Box will turn green when it's on target",
            str wallFigure <+> str "  Wall ",
            str "You can't move through it",
            str holeFigure <+> str "  Hole ",
            str "Box or user will fall into it",
            str fragileFigure <+> str "  Fragile Floor " ,
            str "You can only step on it once",
            str iceFigure <+> str "  Ice Floor " ,
            str "Box will move one step further",
            withAttr doorAttr (str doorFigure) <+> str "  Door",
            str "You can only move through it when the switch is on",
            withAttr switchAttr (str switchFigure) <+> str "  Switch ",
            str "Triggered when something is on it",
            str " ╗ " <+> str "  Rail ",
             str "You can only move in the direction of the rail"
          ]

moveSelection :: Brick.Direction -> AppState -> EventM () AppState ()
moveSelection dir as = do
  let idx = getMapIdx as
  let newIdx = if dir == Up then max 0 (idx - 1) else min (length mapNames - 1) (idx + 1)
  put $ updateMapIdx as newIdx

loadMap :: Int -> Game
loadMap idx = allMaps !! idx

movePlayer1 :: So.Direction -> EventM () AppState ()
movePlayer1 direction = do
  as <- get
  let gs = getGame1 as
  let gs' = step direction gs
  let updatedAppState = updateGame1 as gs'
  if isGameSuccessful gs' || isGameFailed gs'
    then do
      put $ updateGame1 updatedAppState (haltTimer $ getGame1 updatedAppState)
    else do
      put updatedAppState

movePlayer2 :: So.Direction -> EventM () AppState ()
movePlayer2 direction = do
  as <- get
  let gs = getGame2 as
  let gs' = step direction gs
  let updatedAppState = updateGame2 as gs'
  if isGameSuccessful gs' || isGameFailed gs'
    then do
      put $ updateGame2 updatedAppState (haltTimer $ getGame2 updatedAppState)
    else do
      put updatedAppState

restartGame :: EventM () AppState ()
restartGame = put initialState

-- Constants for the game board size and positions
boardSize :: Int
boardSize = 10 -- Change this value to your desired board size

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
title =
  hBox
    [ padRight (Pad 2) $ drawAscii asciiS,
      padRight (Pad 2) $ drawAscii asciiO,
      padRight (Pad 2) $ drawAscii asciiK,
      padRight (Pad 2) $ drawAscii asciiO,
      padRight (Pad 2) $ drawAscii asciiB,
      padRight (Pad 2) $ drawAscii asciiA,
      padRight (Pad 2) $ drawAscii asciiN
    ]
