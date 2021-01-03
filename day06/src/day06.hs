import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List.Extra (dropEnd, takeEnd)
import Data.List.Split (splitOn)

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day06" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input6.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 6a" $
                solvePart1 strs
                `shouldBe`
                400410
            it "passes 6b" $
                solvePart2 strs
                `shouldBe`
                15343601

type Pos = (Int, Int)

type State = [StateRow]
type StateRow = [Bool]

data Op = Off | On | Toggle deriving (Show, Eq)
data Instr = Instr Op Pos Pos deriving (Show, Eq)


solvePart1 :: [String] -> Int
solvePart1 = length . filter id . concat . foldl (\acc instr -> applyInstr acc instr) initState . map toInstr

solvePart2 :: [String] -> Int
solvePart2 = sum . concat . foldl (\acc instr -> applyInstrBright acc instr) initBright . map toInstr

toInstr :: String -> Instr
toInstr str | op == ["toggle"] = Instr Toggle startPos endPos
            | op == ["turn", "on"] = Instr On startPos endPos
            | op == ["turn", "off"] = Instr Off startPos endPos
            | otherwise = error "Bad instr"
    where
        wds = words str
        op = dropEnd 3 wds
        ranges = takeEnd 3 wds

        firstRange = head ranges
        startNums :: [Int]
        startNums = map (read) $ splitOn "," firstRange
        startPos = (,) (head startNums) (last startNums)

        secondRange = last ranges
        endNums :: [Int]
        endNums = map (read) $ splitOn "," secondRange
        endPos = (,) (head endNums) (last endNums)


initState :: State
initState = take 1000 $ repeat (take 1000 $ repeat False)

testState :: State
testState = take 3 $ repeat (take 3 $ repeat False)

applyInstr :: State -> Instr -> State
applyInstr state (Instr op (sCol, sRow) (eCol, eRow)) = take sRow state ++ aux (drop sRow state) op sCol eCol ((eRow - sRow) + 1)
    where
        aux :: State -> Op -> Int -> Int -> Int -> State
        aux state _ _ _ 0 = state
        aux (r:rs) op sCol eCol n = applyInstrRow r op sCol eCol : aux rs op sCol eCol (n - 1)

applyInstrRow :: StateRow -> Op -> Int -> Int -> StateRow
applyInstrRow row op sCol eCol = take sCol row ++ aux (drop sCol row) op ((eCol - sCol) + 1)
    where
        aux :: StateRow -> Op -> Int -> StateRow
        aux row _ 0 = row
        aux (x:xs) op n | op == Off = False : aux xs op (n - 1)
                        | op == On = True : aux xs op (n - 1)
                        | op == Toggle && x == True = False : aux xs op (n - 1)
                        | op == Toggle && x == False = True : aux xs op (n - 1)
                        | otherwise = error "Bad Instr"

type BrightState = [[Int]]
type BrightRow = [Int]

initBright :: BrightState
initBright = take 1000 $ repeat (take 1000 $ repeat 0)

applyInstrBright :: BrightState -> Instr -> BrightState
applyInstrBright state (Instr op (sCol, sRow) (eCol, eRow)) = take sRow state ++ aux (drop sRow state) op sCol eCol ((eRow - sRow) + 1)
    where
        aux :: BrightState -> Op -> Int -> Int -> Int -> BrightState
        aux state _ _ _ 0 = state
        aux (r:rs) op sCol eCol n = applyInstrRowBright r op sCol eCol : aux rs op sCol eCol (n - 1)

applyInstrRowBright :: BrightRow -> Op -> Int -> Int -> BrightRow
applyInstrRowBright row op sCol eCol = take sCol row ++ aux (drop sCol row) op ((eCol - sCol) + 1)
    where
        aux :: BrightRow -> Op -> Int -> BrightRow
        aux row _ 0 = row
        aux (x:xs) op n | op == Off = (max 0 (x - 1)) : aux xs op (n - 1)
                        | op == On = (x + 1) : aux xs op (n - 1)
                        | op == Toggle = (x + 2) : aux xs op (n - 1)
                        | otherwise = error "Bad Instr"
