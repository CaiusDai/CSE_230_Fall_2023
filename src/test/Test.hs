module Main where
import Sokoban as So 
import Prelude hiding (Left, Right)
import Control.Lens hiding ((<|), (|>), (:>), (:<), holes)
import Linear.V2 (V2(..))
import Data.Sequence (Seq(..), (<|), elemIndexL, update)
import qualified Data.Sequence as S
import Data.Set (fromList)
import Data.Foldable (toList,length)
import Data.Maybe (isJust)
import qualified Data.List as L
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit


stepTest :: (Direction -> Game -> Game) -> [Direction] -> Game -> Game 
stepTest move ls g0 = foldr move g0 ls' 
    where 
        ls' = reverse ls

-- simpleBoxTest
desp00     = "player moves up" 
insns00    = [up] 
success00  = checkSuccess (stepTest step insns00 simpleBox)
grndTruth00= False 

desp01    = "player moves down " 
insns01    = [down] 
success01  = checkSuccess (stepTest step insns01 simpleBox)
grndTruth01= False 

desp02     = "player moves right - two boxes" 
insns02    = [right] 
success02  = checkSuccess (stepTest step insns02 simpleBox)
grndTruth02= False 

desp03     = "player moves right - wall" 
insns03    = [left] 
success03  = checkSuccess (stepTest step insns03 simpleBox)
grndTruth03= False 

desp04     = "one box hits" 
insns04    = [down, right, up] 
success04  = checkSuccess (stepTest step insns04 simpleBox)
grndTruth04= False 

desp05     = "one box hits - success" 
insns05    = [down, right, up, down, right, up] 
success05  = checkSuccess (stepTest step insns05 simpleBox)
grndTruth05= True


-- classicBoxTest
desp10     = "hits up"
insns10    = [up] 
success10  = checkSuccess (stepTest step insns10 classicBox )
grndTruth10= False 

desp11    = "hits left"
insns11    = [left] 
success11  = checkSuccess (stepTest step insns11 classicBox )
grndTruth11= False 

desp12     = "hits down"
insns12    = [down] 
success12  = checkSuccess (stepTest step insns12 classicBox )
grndTruth12= False 

desp13     = "hits right"
insns13    = [right] 
success13  = checkSuccess (stepTest step insns13 classicBox )
grndTruth13= False 

desp14     = "hits all"
insns14    = [up, down, left, right, down, up, right] 
success14  = checkSuccess (stepTest step insns14 classicBox )
grndTruth14= True


-- -- mordenBoxCheck 
desp20     = "all matches"
insns20    = [up, right, up,left, down, down, down, left, down, right]
success20  = checkSuccess (stepTest step insns20 mordenBox )
grndTruth20= True

desp21     = "do not match"
insns21    = [left, up, right, up, right, down, down, down, up, left, left, down, down, right, up, up, up, right, up, left] 
success21  = checkSuccess (stepTest step insns14 mordenBox )
grndTruth21= False


-- -- wildCardBoxCheck
desp30     = "wild card box hits"
insns30    = [up, up, left]
success30  = checkSuccess (stepTest step insns30 wildCardBox )
grndTruth30= False

desp31     = "real box hits "
insns31    = [left, left, up]
success31  = checkSuccess (stepTest step insns31 wildCardBox )
grndTruth31= True

-- railBoxCheck
desp40     = "rail blocks - outside"
prev40     = stepTest step [down, right, up, right] railBox
post40     = stepTest step [down, right, up, right, down] railBox

desp41     = "rail blocks (horizontal) - inside"
prev41     = stepTest step [right, down, left, down, right] railBox
post41     = stepTest step [right, down, left, down, right, right] railBox

desp42     = "rail blocks (vertical) - inside"
prev42     = stepTest step [right, down, left, down, right, up, right, down, right, down] railBox
post42     = stepTest step [right, down, left, down, right, up, right, down, right, down,left] railBox


desp43     = "pass rail"
insns43    = [right, down, left, down, right, up, right, down, down, down, left, down, right, up, right, down]
success43  = checkSuccess (stepTest step insns43 railBox )
grndTruth43= True


-- -- icefloorBoxCheck
desp50     = "ice floor Box success"
insns50    = [right, right, down, right, up, right, up, left]
success50 = checkSuccess (stepTest step insns50 icefloorBox)
grndTruth50= True

desp51     = "ice floor Box slide check"
box51     = getBoxes(stepTest step [right] icefloorBox)
grndTruth51     = S.fromList[V2 4 2]

-- -- fragilefloorBoxCheck
desp60     = "fragile floor Box success"
insns60 = [right, right, right, right, down, down, left, left, up, down, right, up, right, up, left, left, down, down, left, up, up]
success60 = checkSuccess (stepTest step insns60 fragilefloorBox)
grndTruth60 = True

desp61     = "fragile floor Box - user in the hole"
insns61 = [right, right, left]
dead61 = getDead (stepTest step insns61 fragilefloorBox)
grndTruth61 = True

desp62     = "fragile floor Box - box in the hole"
insns62 = [right, right, right, right, down, down, left, left, left, up]
dead62 = getDead (stepTest step insns61 fragilefloorBox)
grndTruth62 = True

-- -- doorBoxCheck
desp70     = "door Box success"
insns70 = [right, right, down, right, right, up, left, left, right, right, up, up, left]
success70 = checkSuccess (stepTest step insns70 doorBox)
grndTruth70 = True

desp71     = "door Box - prevent user move"
user71     = getUser(stepTest step [right, right, right, up] doorBox)
grndTruth71     = V2 4 3

desp72     = "door Box - prevent box move"
box72     = getBoxes(stepTest step [right, right, down, right, up] doorBox)
grndTruth72     = S.fromList[V2 4 1, V2 4 3]


simpleBoxTest :: TestTree
simpleBoxTest = testGroup "simpleBoxTest"
  [ testCase desp00 $ success00 == grndTruth00 @? "Test failed",
    testCase desp01 $ success01 == grndTruth01 @? "Test failed",
    testCase desp02 $ success02 == grndTruth02 @? "Test failed",
    testCase desp03 $ success03 == grndTruth03 @? "Test failed",
    testCase desp04 $ success04 == grndTruth04 @? "Test failed",
    testCase desp05 $ success05 == grndTruth05 @? "Test failed"
  ]

classicBoxTest :: TestTree
classicBoxTest = testGroup "classicBoxTest"
  [ testCase desp10 $ success10 == grndTruth10 @? "Test failed",
    testCase desp11 $ success11 == grndTruth11 @? "Test failed",
    testCase desp12 $ success12 == grndTruth12 @? "Test failed",
    testCase desp13 $ success13 == grndTruth13 @? "Test failed",
    testCase desp14 $ success14 == grndTruth14 @? "Test failed"
  ]

mordenBoxCheck  :: TestTree
mordenBoxCheck  = testGroup "classicBoxTest"
  [ testCase desp20 $ success20 == grndTruth20 @? "Test failed",
    testCase desp21 $ success21 == grndTruth21 @? "Test failed"
  ]

wildCardBoxCheck  :: TestTree
wildCardBoxCheck  = testGroup "wildCardBoxCheck"
  [ testCase desp30 $ success30 == grndTruth30 @? "Test failed",
    testCase desp31 $ success31 == grndTruth31 @? "Test failed"
  ]

railBoxCheck  :: TestTree
railBoxCheck  = testGroup "railBoxCheck"
  [ testCase desp40 $ prev40 == post40 @? "Test failed",
    testCase desp41 $ prev41 == post41 @? "Test failed",
    testCase desp42 $ prev42 == post42 @? "Test failed",
    testCase desp43 $ success43 == grndTruth43 @? "Test failed"
  ]

icefloorBoxCheck  :: TestTree
icefloorBoxCheck  = testGroup "icefloorBoxCheck"
  [ testCase desp51 $ box51 == grndTruth51 @? "Test failed",
    testCase desp50 $ success50 == grndTruth50 @? "Test failed"
  ]

fragilefloorBoxCheck  :: TestTree
fragilefloorBoxCheck  = testGroup "fragilefloorBoxCheck"
  [ testCase desp60 $ success60 == grndTruth60 @? "Test failed",
    testCase desp61 $ dead61 == grndTruth61 @? "Test failed",
    testCase desp62 $ dead62 == grndTruth62 @? "Test failed"
  ]

doorBoxCheck  :: TestTree
doorBoxCheck  = testGroup "fragilefloorBoxCheck"
  [ testCase desp71 $ user71 == grndTruth71 @? "Test failed",
    testCase desp72 $ box72 == grndTruth72 @? "Test failed",
    testCase desp70 $ success70 == grndTruth70 @? "Test failed"
  ]


main = defaultMain $ testGroup "All Tests" [simpleBoxTest, classicBoxTest, mordenBoxCheck, wildCardBoxCheck, railBoxCheck, icefloorBoxCheck, fragilefloorBoxCheck, doorBoxCheck]