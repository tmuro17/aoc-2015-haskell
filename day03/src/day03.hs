import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S

import Test.Tasty
import Test.Tasty.Hspec


main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day03" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input3.txt"
    strs <- pure $ T.unpack text
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 3a" $
                solvePart1 strs
                `shouldBe`
                2572
            it "passes 3b" $
                solvePart2 strs
                `shouldBe`
                2631

type Pos = (Int, Int)

solvePart1 :: String -> Int
solvePart1 = length . flip (flip aux (0,0)) S.empty

solvePart2 :: String -> Int
solvePart2 dirs = length united
    where
        bothDirs = zip (cycle "sr") dirs
        santaDirs = map snd $ filter ((== 's') . fst) bothDirs
        roboDirs = map snd $ filter ((== 'r') . fst) bothDirs
        santaSeen = aux santaDirs (0,0) S.empty
        roboSeen = aux roboDirs (0,0) S.empty
        united = S.union santaSeen roboSeen


aux :: String -> Pos -> S.Set Pos -> S.Set Pos
aux [] _ seen = seen
aux (d:ds) pos seen = aux ds (nextPos d pos) (S.insert pos seen)

nextPos :: Char -> Pos -> Pos
nextPos '^' (x, y) = (x, pred y)
nextPos 'v' (x, y) = (x, succ y)
nextPos '>' (x, y) = (succ x, y)
nextPos '<' (x, y) = (pred x, y)
nextPos _ _ = error "Bad direction"

