{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Sokoban (
    b1, b2, b3,
    user, boxes, walls, targets,
    getUser, getBoxes, getTargets, getWall,getScore, getNumTarget,getSteps,getTimer,updateTimer,
    getMenuStatus, updateMenuStatus, getGameMode,updateGameMode, getHoles, getFragiles, getIces,
    getBoxIdx,
    step, checkSuccess,haltTimer,startTimer,
    up, down, left, right,
    Coord(..),
    nextPos, Game(Game), Direction, checkOnTarget,GameMode(Single,Multi)
) where

import Prelude hiding (Left, Right)
import Control.Lens hiding ((<|), (|>), (:>), (:<), holes)
import Linear.V2 (V2(..))
import Data.Sequence (Seq(..), (<|), elemIndexL, update)
import qualified Data.Sequence as S
import Data.Set (fromList)
import Data.Foldable (toList,length)
import Data.Maybe (isJust)

import qualified Data.Map as M


data Game = Game {
    -- components
    _user    :: Coord,
    _box     :: Coord,
    _boxes   :: Seq Coord,
    _walls   :: Seq Coord,
    _target  :: Coord,
    _targets :: Seq Coord,
    _icefloors :: Seq Coord,
    _fragileFloors :: Seq Coord,
    _holes         :: Seq Coord,
    _doors        :: Seq Coord,     
    _switch       :: Coord,         
    _switchState  :: Bool,

    -- _rail :: Seq Coord,
    -- _railDirs :: Seq Coord,  
    
    -- states
    _dir     :: Direction,
    _score  :: Int,
    _suceess :: Bool,
    _dead    :: Bool,
    _num_target:: Int, 

    -- boxes update
    _boxCat  :: Seq String,
    _boxIdx  :: IndexMap,
    _num_steps:: Int,
    _timer_seconds   :: Int,
    _timer_running   :: Bool,

    -- menu related
    _in_menu :: Bool,
    _game_mode :: GameMode
} deriving (Show)

data GameMode = Single | Multi deriving (Show,Eq)

type Coord = V2 Int

type IndexMap = M.Map String (Seq Int)


data Direction = Up | Down | Left | Right deriving (Show, Eq)

makeLenses ''Game

-- next pos of user
nextPos :: Direction -> Coord -> Coord
nextPos Up    (V2 x y) = V2 x (y-1)
nextPos Down  (V2 x y) = V2 x (y+1)
nextPos Left  (V2 x y) = V2 (x-1) y
nextPos Right (V2 x y) = V2 (x+1) y
nextPos _ _ = error "Error direction!"

height :: Int
width :: Int
height = 10
width = 10

xm = width `div` 2
ym = height `div` 2
wall = S.fromList [V2 x y | x <- [0..width-1], y <- [0, height-1]] <>
       S.fromList [V2 x y | x <- [0, width-1], y <- [1..height-2]]
target' = V2 (xm - 1) (ym - 1)
targets' = S.fromList [target']
box' = V2 (xm + 1) (ym + 1)
boxes' = S.fromList [box']
num_targets = 1

-- only one category for box

idx1 = S.fromList([0])
b1 :: Game
b1 = Game
        { _user    = V2 xm ym
        , _box     = box'
        , _boxes   = boxes'
        , _walls   = wall
        , _target  = target'
        , _targets = targets'
        , _dir     = Up
        , _score  = 0
        , _suceess = False
        , _dead    = False
        , _num_target = 1
        , _boxCat = S.fromList(["targets"])
        , _boxIdx = M.singleton "targets" idx1
        , _num_steps = 0
        , _timer_seconds = 0
        , _timer_running = False
        , _in_menu = True
        , _game_mode = Single
        }

idx2 = S.fromList([0,1])
b2 :: Game
b2 = Game
        { _user    = V2 xm ym
        , _box     = box'
        , _boxes   = S.fromList[V2 6 4, V2 6 6]
        , _walls   = wall
        , _target  = target'
        , _targets = S.fromList[V2 6 3, V2 6 7]
        , _icefloors = S.fromList [V2 5 4, V2 5 6]
        , _fragileFloors = S.fromList [V2 7 5]
        , _holes         = S.empty
        , _dir     = Up
        , _score  = 0
        , _suceess = False
        , _dead    = False
        , _num_target = 2
        , _boxCat = S.fromList(["targets"])
        , _boxIdx = M.singleton "targets" idx2
        , _num_steps = 0
        , _timer_seconds = 0
        , _timer_running = False
        , _in_menu = True
        , _game_mode = Single
        }


boxidx :: IndexMap
redidx = S.fromList [0]
blueidx = S.fromList [1, 2]
empty = M.empty
boxidx = M.insert "red" redidx . M.insert "blue" blueidx $ empty

idx3 = S.fromList([0,1,2])
b3 :: Game
b3 = Game
        { _user    = V2 xm ym
        , _box     = box'
        , _boxes   = S.fromList[V2 4 5, V2 6 4, V2 6 6, V2 6 2, V2 5 7]
        -- ,_boxes   = S.fromList[V2 6 6, V2 4 6, V2 5 6, V2 2 3, V2 3 3, V2 4 3]
        , _walls   = wall
        , _target  = target'
        , _targets = S.fromList[V2 3 5, V2 6 3, V2 6 7]
        -- , _targets = S.fromList[V2 6 6, V2 4 6, V2 5 6]
        -- ,_targets = S.fromList[V2 2 3, V2 3 3, V2 4 3]
        , _icefloors = S.fromList [V2 5 4, V2 5 8]
        , _fragileFloors = S.fromList [V2 7 5]
        , _holes         = S.empty
        ,_doors        =  S.fromList [V2 5 6]   
        ,_switch       =  V2 7 4       
        ,_switchState  = False
        , _dir     = Up
        , _score  = 0
        , _suceess = False
        , _dead    = False
        , _num_target = 3
        -- boxes update
        , _boxCat = S.fromList(["red","blue"])
        , _boxIdx = boxidx
        , _num_steps = 0
        , _timer_seconds = 0
        , _timer_running = False
        , _in_menu = True
        , _game_mode = Single
        }


findIndex :: Coord -> Seq Coord -> Maybe Int
findIndex = elemIndexL 

moveBox :: Int -> Coord -> Seq Coord -> Seq Coord
moveBox  = update


indices2Seq :: Seq Int -> Seq Coord -> Seq Coord
indices2Seq indices seq = S.fromList [seq `S.index` idx | idx <- toList indices]

checkCondition :: Seq Coord -> Seq Coord -> Bool 
checkCondition  seq1 seq2 = 
    let set1 = fromList (toList seq1)
        set2 = fromList (toList seq2) 
    in 
        if set1 == set2 then True else False


checkSuccess :: Game -> Bool
checkSuccess g = checkHelper (M.toList (g^. boxIdx))
  where
    checkHelper [] = True  
    checkHelper ((key, indices):rest) =
      let targetSeq = indices2Seq indices (g^.targets)
          boxesSeq = indices2Seq indices (g^.boxes)
      in 
        if not (checkCondition targetSeq boxesSeq)
            then False  
            else checkHelper rest 


-- Given a sequence of boxes and a sequence of targets, check which boxes are on targets
checkOnTarget :: Seq Coord -> Seq Coord -> Seq Bool
checkOnTarget boxes targets = 
    let targetsList = toList targets  -- Convert targets sequence to list for easy comparison
    in S.fromList [box `elem` targetsList | box <- toList boxes]

-- Check if a box at a given index is in BoxIdx
isBoxInBoxIdx :: Int -> Game -> Bool
isBoxInBoxIdx boxIndex g =
  let boxIdxMap = g^.boxIdx
      boxList = S.index (g^.boxes) boxIndex
  in any (elem boxIndex . snd) (M.toList boxIdxMap)

step :: Direction -> Game -> Game
step d g =
    let nextUserPos = nextPos d (g ^. user)
        isNextWall = findIndex nextUserPos (g ^. walls)
        isNextDoor = findIndex nextUserPos (g ^. doors) /= Nothing && not (g ^. switchState)
        isNextBox = findIndex nextUserPos (g ^. boxes)
        isNextHole = findIndex nextUserPos (g ^. holes)
        isNextFragile = findIndex nextUserPos (g ^. fragileFloors)
        isNextSwitch = nextUserPos == (g ^. switch)
        isBoxOnSwitch = isJust (findIndex (g ^. switch) (g ^. boxes))
        isUserOrBoxOnSwitch = isNextSwitch || isBoxOnSwitch

        updateFragileAndHole pos 
            | isJust (findIndex pos (g ^. fragileFloors)) = 
                g & fragileFloors %~ S.filter (/= pos)
                  & holes %~ (S.|> pos)
            | otherwise = g

        moveBoxToNextPos boxPos =
            let nextBoxPos = nextPos d boxPos
                isNextBoxPosDoor = findIndex nextBoxPos (g ^. doors) /= Nothing && not (g ^. switchState)
                isNextBoxPosWall = findIndex nextBoxPos (g ^. walls) /= Nothing
                isNextBoxPosBox = isJust (findIndex nextBoxPos (g ^. boxes))
            in if isNextBoxPosWall || isNextBoxPosDoor || isNextBoxPosBox
               then boxPos
               else nextBoxPos

        isAnyBoxOnSwitch = any (\boxPos -> boxPos == (g ^. switch)) (toList (g ^. boxes))
        isSwitchActive = (g ^. user) == (g ^. switch) || isAnyBoxOnSwitch

    in case (isNextWall, isNextDoor) of
        (Just _, _) -> g -- User cannot move into a wall
        (_, True) -> g -- User cannot move into a closed door
        _ ->
            case (isNextBox, isNextHole) of
                (Just boxIndex, _) ->
                    let nextBoxPos = nextUserPos
                        finalBoxPos = moveBoxToNextPos nextBoxPos
                        isNextNextWall = findIndex finalBoxPos (g ^. walls)
                        isNextNextBox = findIndex finalBoxPos (g ^. boxes)
                        isNextNextHole = findIndex finalBoxPos (g ^. holes)
                    in
                        case (isNextNextWall, isNextNextBox, isNextNextHole) of
                            (Nothing, Nothing, Just holeIndex) ->
                                let isBoxIdx = isBoxInBoxIdx boxIndex g
                                in if isBoxIdx
                                    then g & dead .~ True
                                    else g & boxes .~ S.deleteAt boxIndex (g ^. boxes)
                                           & holes .~ S.deleteAt holeIndex (g ^. holes)
                                           & user .~ nextUserPos
                            (Nothing, Nothing, _) ->
                                if finalBoxPos == nextBoxPos
                                then g -- Box cannot be moved (due to door or other box)
                                else updateFragileAndHole nextUserPos
                                     & user .~ nextUserPos
                                     & boxes .~ (update boxIndex finalBoxPos (g ^. boxes))
                                     & switchState .~ isSwitchActive
                            _ -> g -- Movement blocked by a wall or another box
                (Nothing, Just _) ->
                    updateFragileAndHole nextUserPos
                    & user .~ nextUserPos
                    & dead .~ True
                (Nothing, _) ->
                    let g' = updateFragileAndHole nextUserPos
                    in g' & user .~ nextUserPos
                          & switchState .~ isSwitchActive

-- Getters and Setters
getUser :: Game -> Coord
getUser g = g^.user

getBoxes :: Game -> Seq Coord
getBoxes g = g^.boxes 

getTargets :: Game -> Seq Coord 
getTargets g = g^. targets

getWall :: Game -> Seq Coord 
getWall g = g^. walls

getHoles :: Game -> Seq Coord 
getHoles g = g^. holes

getFragiles :: Game -> Seq Coord
getFragiles g = g^. fragileFloors

getIces :: Game -> Seq Coord
getIces g = g^. icefloors

getNumTarget :: Game -> Int
getNumTarget g = g^. num_target

getScore :: Game -> Int
getScore g = let boxesOnTargets = checkOnTarget (getBoxes g) (getTargets g)
                in length $ filter id $ toList boxesOnTargets

getBoxCat :: Game -> Seq String 
getBoxCat g = g^. boxCat

getBoxIdx :: Game -> IndexMap 
getBoxIdx g = g^.boxIdx

getSteps :: Game -> Int
getSteps g = g^. num_steps

getTimer :: Game -> Int
getTimer g = g^. timer_seconds

getMenuStatus :: Game -> Bool
getMenuStatus g = g^. in_menu

updateMenuStatus :: Game -> Bool -> Game
updateMenuStatus g status = g & in_menu .~ status

getGameMode :: Game -> GameMode
getGameMode g = g^. game_mode

updateGameMode :: Game -> GameMode -> Game
updateGameMode g mode = g & game_mode .~ mode

updateTimer :: Game -> Game
updateTimer g = if g ^. timer_running 
                then g & timer_seconds .~ (g ^. timer_seconds + 1)
                else g

haltTimer :: Game -> Game
haltTimer g = g & timer_running .~ False

startTimer :: Game -> Game
startTimer g = g & timer_running .~ True
up :: Direction 
up = Up 

down :: Direction
down = Down 

left :: Direction
left = Left 

right :: Direction
right = Right
