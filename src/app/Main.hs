module Main where

import Brick
import Brick.Widgets.Border
import UI
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Control.Monad
import Brick.BChan
data App 

myWidget :: Widget ()
myWidget = border $ hBox $ replicate 3 (str " ")

main :: IO ()
main = do
  chan <- newBChan 10
  _ <- startTicking 1000000 chan
  let builder = mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app initialState
 

--- Tests about unicode
