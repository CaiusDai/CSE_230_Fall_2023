{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Sokoban (
    b1, b2,
    user, boxes, walls, targets,
    getUser, getBoxes, getTargets, getWall,getScore, getNumTarget,
    step, checkSuccess,
    up, down, left, right,
    nextPos, Game(Game), Direction, checkOnTarget
) where

import Prelude hiding (Left, Right)
import Control.Lens hiding ((<|), (|>), (:>), (:<), holes)
import Linear.V2 (V2(..))
import Data.Sequence (Seq(..), (<|), elemIndexL, update)
import qualified Data.Sequence as S
import Data.Set (fromList)
import Data.Foldable (toList,length)
import Data.Maybe (isJust)

import qualified Data.Map as M hiding (update) 


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
    
    -- states
    _dir     :: Direction,
    _score  :: Int,
    _suceess :: Bool,
    _dead    :: Bool,
    _num_target:: Int, 

    -- boxes update
    _boxCat  :: Seq String,
    _boxIdx  :: IndexMap
} deriving (Show)

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
        }


boxidx :: IndexMap
redidx = S.fromList [0]
blueidx = S.fromList [1, 2]
empty = M.empty
boxidx = M.insert "red" redidx . M.insert "blue" blueidx $ empty

targets3 = S.fromList[V2 3 5, V2 6 3, V2 6 7]
idx3 = S.fromList([0,1,2])
b4 :: Game
b4 = Game
        { _user    = V2 xm ym
        , _box     = box'
        , _boxes   = S.fromList[V2 4 5, V2 6 4, V2 6 6, V2 6 2, V2 5 7]
        , _walls   = wall
        , _target  = target'
        , _targets = targets3
        , _icefloors = S.fromList [V2 5 4, V2 5 6]
        , _fragileFloors = S.fromList [V2 7 5]
        , _holes         = S.empty
        , _dir     = Up
        , _score  = 0
        , _suceess = False
        , _dead    = False
        , _num_target = 2
        -- boxes update
        , _boxCat = S.fromList(["red","blue"])
        , _boxIdx = boxidx
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

step :: Direction -> Game -> Game
step d g =
    let nextUserPos = nextPos d (g ^. user)
        isNextWall = findIndex nextUserPos (g ^. walls)
        isNextBox = findIndex nextUserPos (g ^. boxes)
        isNextHole = findIndex nextUserPos (g ^. holes)
        isNextFragile = findIndex nextUserPos (g ^. fragileFloors)
        updateFragileAndHole pos 
            | isJust (findIndex pos (g ^. fragileFloors)) = 
                g & fragileFloors %~ S.filter (/= pos)
                  & holes %~ (S.|> pos)
            | otherwise = g
        moveBoxToNextPos boxPos =
            let nextBoxPos = nextPos d boxPos
            in if findIndex nextBoxPos (g ^. icefloors) /= Nothing
               then moveBoxToNextPos nextBoxPos
               else nextBoxPos
    in
        case isNextWall of
            Just _ -> g -- User cannot move into a wall
            Nothing ->
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
                                    -- Box is pushed into a hole, remove the box and the hole
                                    g & boxes .~ S.deleteAt boxIndex (g ^. boxes)
                                      & holes .~ S.deleteAt holeIndex (g ^. holes)
                                (Nothing, Nothing, _) ->
                                    -- move user, move box
                                    updateFragileAndHole nextUserPos
                                    & user .~ nextUserPos
                                    & boxes .~ (update boxIndex finalBoxPos (g ^. boxes))
                                _ -> g -- Movement blocked by a wall or another box
                    (Nothing, Just _) ->
                        -- User steps into a hole
                        updateFragileAndHole nextUserPos
                        & user .~ nextUserPos
                        & dead .~ True
                    (Nothing, _) ->
                        -- Normal movement
                        let g' = updateFragileAndHole nextUserPos
                        in g' & user .~ nextUserPos
            _ -> g

-- check 
getUser :: Game -> Coord
getUser g = g^.user

getBoxes :: Game -> Seq Coord
getBoxes g = g^.boxes 

getTargets :: Game -> Seq Coord 
getTargets g = g^. targets

getWall :: Game -> Seq Coord 
getWall g = g^. walls

getNumTarget :: Game -> Int
getNumTarget g = g^. num_target

getScore :: Game -> Int
getScore g = let boxesOnTargets = checkOnTarget (getBoxes g) (getTargets g)
                in length $ filter id $ toList boxesOnTargets

getBoxCat :: Game -> Seq String 
getBoxCat g = g^. boxCat

getBoxIdx :: Game -> IndexMap 
getBoxIdx g = g^.boxIdx

up :: Direction 
up = Up 

down :: Direction
down = Down 

left :: Direction
left = Left 

right :: Direction
right = Right
