import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Test.Tasty
import Test.Tasty.Hspec
import Control.Conditional

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day14" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input14.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 14a" $
                solvePart1 strs
                `shouldBe`
                2655
            it "passes 14b" $
                solvePart2 strs
                `shouldBe`
                1059


--                   Name Speed Stamina Rest
data DeerInfo = Deer String Int Int Int deriving (Show, Eq)

solvePart1 :: [String] -> Int
solvePart1 = maximum . map (head . drop 2502 . raceSeconds . parseDeerInfo)
--solvePart1 s = maximum $ map (head . drop 2502 . raceSeconds) $ map parseDeerInfo s

solvePart2 :: [String] -> Int
solvePart2 = maximum . scoreRace . deerPositions . map parseDeerInfo
--solvePart2 s = maximum $ scoreRace $ deerPositions (map parseDeerInfo s)

parseDeerInfo :: String -> DeerInfo
parseDeerInfo s = Deer name speed stamina rest
    where
        parts = words s
        name = head parts
        speed = read $ head $ drop 3 parts
        stamina = read $ head $ drop 6 parts
        rest = read $ head $ drop 13 parts


raceSeconds :: DeerInfo -> [Int]
raceSeconds deer@(Deer _ _ dS dR) = aux deer dS dR 0
    where 
        aux :: DeerInfo -> Int -> Int -> Int -> [Int]
        aux d@(Deer _ dSpeed dStamina dRest) 0 0 position = (dSpeed + position) : (aux d (pred dStamina) dRest (dSpeed + position))
        aux d 0 rest position = position : (aux d 0 (pred rest) position)
        aux d@(Deer _ dSpeed _ _) stamina rest position = (position + dSpeed) : (aux d (pred stamina) rest (position + dSpeed))

deerPositions :: [DeerInfo] -> [[Int]]
deerPositions = map (take 2503 . raceSeconds)

scoreRace :: [[Int]] -> [Int]
scoreRace = aux =<< ($ repeat 0) . take . length
--scoreRace positions = aux (take (length positions) $ repeat 0) positions
    where
        aux :: [Int] -> [[Int]] -> [Int]
        aux score pos | concat pos == [] = score
                      | otherwise = aux nextScore tailPos
            where
                headPos = map head pos
                tailPos = map tail pos
                nextScore = scorePosition score headPos

scorePosition :: [Int] -> [Int] -> [Int]
scorePosition score positions = newScore
    where
        maxPos = maximum positions
        mappedSlice = map (flip (flip if' 1 . (maxPos ==)) 0) positions
--        mappedSlice = map (\x -> if x == maxPos then 1 else 0) positions
        newScore =  zipWith (+) score mappedSlice
