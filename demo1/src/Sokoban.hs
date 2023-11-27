module Sokoban(
    test1
)where 

-- import Main(main)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Linear.V2 (V2(..), _x, _y)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Data.Maybe (fromMaybe)
import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Data.Set (fromList)

data Game = Game {
    -- components
        _user   :: Coord,
        _box    :: Coord,
        _boxes  :: Seq Coord,
        _walls  :: Seq Coord,
        _target :: Coord,
        _targets:: Seq Coord,
    -- states
        _dir    :: Direction,
        -- _score  :: Int,
        _dead   :: Bool
}deriving(Show)


type Coord = V2 Int
data Direction
    = Up 
    | Down 
    | Left 
    | Right 
    deriving(Show, Eq) 



-- step :: Direction -> Game -> Game
-- step d g = do
--     -- get next pos of user  
--     let next = nextPos d (g ^. user)
--         in if elem next (g ^. walls)
--             then g
--             else case findBoxIndex next g of
--                     Nothing -> g & user %~ next
--                     Just boxIndex ->
--                         let nextBox = nextPos d (g ^. boxes !! boxIndex)
--                         in if notElem nextBox (g ^. walls) && notElem nextBox (g ^. boxes)
--                             then g & boxes . element boxIndex %~ (\c -> nextPos d c) & user %~ nextPos d g^.user 
--                             else g


-- next pos of user
nextPos :: Direction -> Coord -> Coord
nextPos Up    pos = pos & y %~ (\y -> y + 1)
nextPos Down  pos = pos & y %~ (\y -> y - 1)
nextPos Left  pos = pos & x %~ (\x -> x - 1)
nextPos Right pos = pos & x %~ (\x -> x + 1)
nextPos _ = error "Error direction!"

-- deadLoc = [V2 0 0, V2 0 (width-1), V2 (height-1) 0, V2 (height-1) ((width-1))]


die :: MaybeT (State Game) ()
die = do
  MaybeT . fmap guard $ elem <$> (nextHead <$> get) <*> (use snake)
  MaybeT . fmap Just $ dead .= True


-- UI
height :: Int 
width  :: Int 
height = 10
width  = 10

xm = width `div` 2
ym = height `div` 2
wall = S.fromList [V2 x y | x <- [0..width-1], y <- [0, height-1]] <>
        S.fromList [V2 x y | x <- [0, width-1], y <- [1..height-2]]
target = V2 (xm - 1) (ym - 1)
targets = S.fromList [target]
box = V2 (xm + 1) (ym + 1)
boxes = S.fromList [box]


test1 :: Game 
test1 = Game
        { _user   = V2 xm ym
        , _box    = box
        , _boxes  = boxes
        , _walls  = wall
        , _target = target
        , _targets = targets
        , _dir    = Up    
        -- , _score  = 0
        }
    

-- Given: pos of user
-- 1. 人前面是空地:人移动
-- 2. 人前面是箱子:
--    1) 箱子的前面不是箱子也不是墙 箱子移动，人移动
-- 其他情况：状态不变

findIndex :: a -> Seq a -> Maybe Int
findIndex element seq = elemIndexL element seq

step :: Direction -> Game -> Game
step d g =
    let nextUserPos = nextPos d (g ^. user)
        isNextWall = findIndex nextUserPos (g ^. walls)
        isNextBox = findIndex nextUserPos (g ^. boxes)
    in
        case isNextBox of
            Nothing ->
                case isNextWall of
                    Nothing -> 
                        -- move
                        g & user .~ nextUserPos
                    Just _ -> 
                        g
                        -- handle collision with wall (do nothing)
            Just nextBoxPos -> 
                -- handle collision with box
                let nextBoxPos = nextPos d nextUserPos 
                    isNextNextWall = findIndex nextBoxPos (g ^. walls)
                    isNextNextBox  = findIndex nextBoxPos (g ^. boxes)
                    isNextNextTarget = findIndex nextBoxPos (g ^. targets)
                in 
                    case isNextNextWall of 
                        Nothing -> 
                            case isNextNextBox of 
                                Nothing -> 
                                    case isNextNextTarget of 
                                        Nothing -> 
                                            -- move user, move box
                                            g & user .~ nextUserPos
                                            g & boxes .~ moveBox isNextBox nextUserPos (g ^. boxes) 
                                        Just targetIndex -> 
                                            -- setTarget 
                                            g & user .~ nextUserPos
                                            g & boxes .~ moveBox isNextBox nextUserPos (g ^. boxes) 

moveBox :: Seq Coord -> Int -> Coord -> Seq Coord 
moveBox boxes index nextBoxPos = update index nextBoxPos boxes

successCheck :: Game -> Bool 
successCheck g = 
    let targetsSet = fromList $ toList g ^. targets 
        boxesSet   = fromList $ toList g ^. boxes 
    in targetsList == boxesList


