module Main where

import Brick
import Brick.Widgets.Border
import UI
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Control.Monad

data App 

myWidget :: Widget ()
myWidget = border $ hBox $ replicate 3 (str " ")

main :: IO ()
main = do
  let builder = mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder Nothing app initialState
