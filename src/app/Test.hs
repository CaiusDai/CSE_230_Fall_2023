module Test(main)
where 
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


-- -- allCheckTest :: [Bool] 
trueCounter :: [Bool]->Int 
trueCounter [] = 0
trueCounter (x:xs) = 
    if x == True 
        then 1 + trueCounter(xs) 
        else trueCounter(xs)


-- helper
checkSequence :: Seq Coord -> Seq Coord -> Bool 
checkSequence  seq1 seq2 = 
    let set1 = fromList (toList seq1)
        set2 = fromList (toList seq2) 
    in 
        if set1 == set2 then True else False

stepTest :: (Direction -> Game -> Game) -> [Direction] -> Game -> Game 
stepTest move ls g0 = foldr move g0 ls' 
    where 
        ls' = reverse ls


-- classicBoxTest
insns1    = [up,down,left,right,right,left,down] 
success1  = checkSuccess (stepTest step insns1 classicBox )

-- -- mordenBoxCheck 
insns2 = [up, right, up,left, down, down, down, left, down, right]
success2 = checkSuccess (stepTest step insns2 mordenBox)

-- -- wildCardBoxCheck
insns3 = [left, left, up]
success3 = checkSuccess (stepTest step insns3 wildCardBox)

-- -- railBoxCheck
insns4 = [right, down, left, down, right, up, right, down, down, down, left, down, right, up, right, down]
success4 = checkSuccess (stepTest step insns3 railBox)


testCases = [(insns1, classicBox),
             (insns2, mordenBox),
             (insns3, wildCardBox),
             (insns4, railBox)]

grndTruth = [True, True, True, True]

finalGame = map (\(insn,game)-> stepTest step insn game) testCases

checkSuccessCases = map checkSuccess finalGame 

checkTestCases ls1 ls2 = case (ls1,ls2) of  
    (x1:xs1, x2:xs2) -> (x1 == x2) : checkTestCases xs1 xs2 
    (_,_) -> []

result = checkTestCases checkSuccessCases grndTruth

main :: IO ()
main = do 
    putStrLn ("pass test case: " ++ show (trueCounter result) ++ "/" ++ show (length result))



