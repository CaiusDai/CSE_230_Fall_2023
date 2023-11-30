{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Test (
    -- * Data Types
    Game(..),
    Direction(..),

    -- * Functions
    initGame,
    step,
    turn,
    checkAndUpdateSuccess,
    updateGameDead
) where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Linear.V2 (V2(..), _x, _y)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Data.Maybe (fromMaybe)
import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Data.Foldable (toList)
import Data.List
import Prelude hiding (Left, Right)

data Game = Game {
    -- components
        _user   :: Coord,
        _boxes  :: Seq Coord,
        _walls  :: Seq Coord,
        _targets:: Seq Coord,
        _success:: Bool,
        _dir    :: Direction,
        _dead   :: Bool
}deriving(Show)


type Coord = V2 Int
data Direction
    = Up 
    | Down 
    | Left 
    | Right 
    deriving(Show, Eq) 

makeLenses ''Game

step :: Direction -> Game -> Game
step d g =
    let nextUserPos = nextPos d (g ^. user)
        updateGameWithBox boxIndex = 
            case S.lookup boxIndex (g ^. boxes) of
                Just boxPos ->
                    let nextBoxPos = nextPos d boxPos
                    in if isFree nextBoxPos g
                       then g & boxes . ix boxIndex %~ nextPos d & user .~ nextUserPos
                       else g
                Nothing -> g
    in if isWall nextUserPos g
       then g
       else case findBoxIndex nextUserPos g of
            Nothing -> g & user .~ nextUserPos
            Just boxIndex -> updateGameWithBox boxIndex

isWall :: Coord -> Game -> Bool
isWall pos g = elem pos (g ^. walls)

isFree :: Coord -> Game -> Bool
isFree pos g = notElem pos (g ^. walls) && notElem pos (g ^. boxes)

nextPos :: Direction -> Coord -> Coord
nextPos Up    pos = pos & _y %~ (\y -> y + 1)
nextPos Down  pos = pos & _y %~ (\y -> y - 1)
nextPos Left  pos = pos & _x %~ (\x -> x - 1)
nextPos Right pos = pos & _x %~ (\x -> x + 1)
nextPos _     _   = error "Invalid direction"

findBoxIndex :: Coord -> Game -> Maybe Int
findBoxIndex targetBoxCoord game = S.elemIndexL targetBoxCoord (_boxes game)


checkAndUpdateSuccess :: Game -> Game
checkAndUpdateSuccess game =
    let boxesList = toList $ _boxes game
        targetsList = toList $ _targets game
        isSuccess = null (boxesList \\ targetsList) && null (targetsList \\ boxesList)
    in game & success .~ isSuccess

deadLoc :: [Coord]
deadLoc = [V2 0 0, V2 0 (width-1), V2 (height-1) 0, V2 (height-1) ((width-1))]

updateGameDead :: Game -> Game
updateGameDead game = 
    let boxesList = toList $ _boxes game
        isDead = any (`elem` deadLoc) boxesList
    in game & dead .~ isDead

turn :: Direction -> Game -> Game
turn newDir game =
    let gameWithNewDir = game & dir .~ newDir
        gameAfterStep = step newDir gameWithNewDir
        gameAfterSuccessCheck = checkAndUpdateSuccess gameAfterStep
    in updateGameDead gameAfterSuccessCheck

height :: Int 
width  :: Int 
height = 10
width  = 10

xm = width `div` 2
ym = height `div` 2
wall = S.fromList [V2 x y | x <- [0..width-1], y <- [0, height-1]] <>
        S.fromList [V2 x y | x <- [0, width-1], y <- [1..height-2]]
target = V2 (xm - 1) (ym - 1)
targetsSample = S.fromList [target]
box = V2 (xm + 1) (ym + 1)
boxesSample = S.fromList [box]

initGame :: IO Game
initGame = return $ Game
        { _user   = V2 xm ym
        , _boxes  = boxesSample
        , _walls  = wall
        , _targets = targetsSample
        , _success = False
        , _dir    = Up 
        , _dead = False   
        }


-- type State = (Coord, Coord, Int) -- (Player Position, Box Position, Moves)

-- -- Helper functions to check valid positions and map 2D coordinates to 1D
-- isValid :: Coord -> Bool
-- isValid pos@(V2 i j) = i >= 0 && i < height && j >= 0 && j < width && notElem pos (toList $ _walls game)

-- to1D :: Coord -> Int
-- to1D (V2 i j) = i * width + j

-- -- Directions
-- directions :: [Coord]
-- directions = [V2 (-1) 0, V2 0 (-1), V2 1 0, V2 0 1]

-- -- BFS function
-- bfsToTarget :: Game -> Seq State -> Set (Int, Int) -> Coord -> Int
-- bfsToTarget game states visited target
--     | Seq.null states = -1
--     | otherwise =
--         let ((V2 si sj, V2 bi bj, d):<|rest) = states
--             newStates = catMaybes $ map (nextState game (V2 si sj, V2 bi bj, d)) directions
--             notVisited (V2 si' sj', V2 bi' bj', _) = not $ Set.member (to1D $ V2 si' sj', to1D $ V2 bi' bj') visited
--             filteredStates = filter notVisited newStates
--             newVisited = foldl (\acc (V2 si' sj', V2 bi' bj', _) -> Set.insert (to1D $ V2 si' sj', to1D $ V2 bi' bj') acc) visited filteredStates
--         in if any (\(_, boxPos, _) -> boxPos == target) newStates then d
--            else bfsToTarget game (rest <> Seq.fromList filteredStates) newVisited target

-- -- Generate next state
-- nextState :: Game -> State -> Coord -> Maybe State
-- nextState game (playerPos, boxPos, d) move =
--     let newPlayerPos = playerPos + move
--     in if isValid newPlayerPos then
--         if newPlayerPos == boxPos
--         then let newBoxPos = boxPos + move
--              in if isValid newBoxPos then Just (newPlayerPos, newBoxPos, d + 1) else Nothing
--         else Just (newPlayerPos, boxPos, d)
--     else Nothing

-- -- Main function to start BFS
-- minPushBox :: Game -> Int
-- minPushBox game =
--     let player = _user game
--         box = head (toList $ _boxes game)
--         target = head (toList $ _targets game)
--         initialVisited = Set.singleton (to1D player, to1D box)
--         initialState = Seq.singleton (player, box, 0)
--     in bfsToTarget game initialState initialVisited target
       