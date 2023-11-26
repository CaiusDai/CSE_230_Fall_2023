module Sokoban(
    test
)where 

import Main(main)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Sequence (Seq(..), (<|))
import Linear.V2 (V2(..), _x, _y)


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
}


type Coord = V2 Int
data Direction
    = Up 
    | Down 
    | Left 
    | Right 
    deriving(Show, Eq) 




step :: Game -> Game 



occupyTarget :: MaybeT (State Game) 
occupyTarget = do 


-- UI
height :: Int 
width  :: Int 
height = 20 
width  = 20 

initGame :: IO Game 
initGame = do 
    let xm = width `div` 2
        ym = height `div` 2
        g  = Game { 
            _snake  = (S.singleton (V2 xm ym))
            , _food   = f
            , _foods  = fs
            , _score  = 0
            , _dir    = North
            , _dead   = False
            , _paused = True
            , _locked = False
        }
    return $ execState nextFood g
