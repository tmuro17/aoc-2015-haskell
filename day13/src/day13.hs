import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day13" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input13.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 13a" $
                solvePart1 strs
                `shouldBe`
                709
            it "passes 13b" $
                solvePart2 strs
                `shouldBe`
                668



solvePart1 :: [String] -> Int
solvePart1 = liftM2 optimize parseToRuleMap (allPeople . parseToRuleMap)

solvePart2 :: [String] -> Int
solvePart2 = liftM2 optimize parseToRuleMap (("You":) . allPeople . parseToRuleMap)


type RuleMap = M.Map (S.Set String) Int

addRule :: RuleMap -> String -> RuleMap
addRule m s = M.insertWith (+) (S.fromList [to, from]) signedAmount m
    where
        parts = words s
        to = head parts
        from = init $ last parts
        operative = head $ drop 2 parts
        amount = read $ head $ drop 3 parts
        signedAmount = if operative == "gain" then amount else (negate amount)

parseToRuleMap :: [String] -> RuleMap
parseToRuleMap = foldr (flip addRule) M.empty

cost :: RuleMap -> [String] -> Int
cost rm people = aux rm people [] 0
    where
    aux :: RuleMap -> [String] -> [String] -> Int -> Int
    aux _ [] _ tot = tot
    aux _ [_] [] tot = tot
    aux m [x] seen tot = tot + M.findWithDefault 0 (S.fromList [x, last seen]) m
    aux m (x:y:rst) seen tot = aux m (y:rst) (x:seen) (tot + current)
        where
            current = M.findWithDefault 0 (S.fromList [x, y]) m

allPeople :: RuleMap -> [String]
allPeople = S.toList . foldr S.union S.empty . M.keys

optimize :: RuleMap -> [String] -> Int
optimize = (maximum .) . (. permutations) . map . cost
--optimize rm people = maximum $ map (cost rm) $ permutations people

