import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
import Control.Monad

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day09" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input9.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 9a" $
                solvePart1 strs
                `shouldBe`
                141
            it "passes 9b" $
                solvePart2 strs
                `shouldBe`
                736

solvePart1 :: [String] -> Int
solvePart1 = smallestCost . processPathMap

solvePart2 :: [String] -> Int
solvePart2 = largestCost . processPathMap

processPathMap :: [String] -> PathMap
processPathMap = foldr addEntryToPathMap M.empty

type PathMap = M.Map String [(String, Int)]

addEntryToPathMap :: String -> PathMap -> PathMap
addEntryToPathMap str m = insertBoth m src dst wgt
    where
        wds = words str
        src = head wds
        dst = head $ drop 2 wds
        wgt = read $ last wds

        aux :: (String, Int) -> [(String, Int)] -> [(String, Int)]
        aux entry [] = [entry]
        aux e es = e:es

        insertBoth :: PathMap -> String -> String -> Int -> PathMap
        insertBoth pm s d cst = case M.lookup src pm of
            Nothing -> case M.lookup d pm of
                Nothing -> M.insert s [(d, cst)] $ M.insert d [(s, cst)] m
                Just des -> M.insert s [(d, cst)] $ M.insert d (aux (s, cst) des) m
            Just ses -> case M.lookup d pm of
                Nothing -> M.insert s (aux (d, cst) ses) $ M.insert d [(s, cst)] m
                Just des -> M.insert s (aux (d, cst) ses) $ M.insert d (aux (s, cst) des) m


smallestCost :: PathMap -> Int
smallestCost = minimum . ap (map . flip flip 0 . flip flip ([]) . aux) M.keys -- from pointfree.io
--smallestCost pm = minimum $ map (\start -> aux pm start [] 0) $ M.keys pm -- Original
    where
        aux :: PathMap -> String -> [String] -> Int -> Int
        aux m start seen cost = case M.lookup start m of
            Nothing -> error "Bad start"
            Just dstInf -> case filter (not . flip elem seen . fst) dstInf of
                [] -> cost
                notSeen -> minimum $ (:) maxBound $ map (\(d, w) -> aux m d (start:seen) (w + cost)) notSeen


largestCost :: PathMap -> Int
largestCost pm = maximum $ map (flip (flip (aux pm) []) 0) $ M.keys pm
    where
        aux :: PathMap -> String -> [String] -> Int -> Int
        aux m start seen cost = case M.lookup start m of
            Nothing -> error "Bad start"
            Just dstInf -> case filter (not . flip elem seen . fst) dstInf of
                [] -> cost
                notSeen -> maximum $ (:) minBound $ map (uncurry ((. (cost +)) . flip (aux m) (start : seen))) notSeen
--                notSeen -> maximum $ (:) minBound $ map (\(d, w) -> aux m d (start:seen) (w + cost)) notSeen
