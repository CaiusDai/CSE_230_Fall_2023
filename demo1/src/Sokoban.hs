module Sokoban(
    test1
)where 

-- import Main(main)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Linear.V2 (V2(..), _x, _y)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S

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
        _score  :: Int,
        _dead   :: Bool
}deriving(Show)


type Coord = V2 Int
data Direction
    = Up 
    | Down 
    | Left 
    | Right 
    deriving(Show, Eq) 




-- UI
height :: Int 
width  :: Int 
height = 6 
width  = 6 

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
        , _score  = 0
        }
    