
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.Map as M
import Control.Monad

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day16" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input16.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 16a" $
                solvePart1 strs
                `shouldBe`
                (SueData 40 $ M.fromList [("akitas", 0), ("cats", 7), ("vizslas", 0)])
            it "passes 16b" $
                solvePart2 strs
                `shouldBe`
                (SueData 241 $ M.fromList [("cars", 2), ("pomeranians", 1), ("samoyeds", 2)])

type Attrs = M.Map String Int

data SueData = SueData Int Attrs deriving (Show, Eq)

solvePart1 :: [String] -> SueData
solvePart1 = head . filter (matchTarget masterSue) . map parseSueData

solvePart2 :: [String] -> SueData
solvePart2 = head . filter (matchTarget2 masterSue) . map parseSueData

masterSue :: SueData
masterSue = SueData 999 $ M.fromList [("children", 3), ("cats", 7), ("samoyeds", 2),
                                      ("pomeranians", 3), ("akitas", 0), ("vizslas", 0),
                                      ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)
                                     ]

parseSueData :: String -> SueData
parseSueData s = SueData sueNum $ aux M.empty $ map (filter (liftM2 (&&) (/= ',') (/= ':'))) unMappedData
    where
        parts = words s
        sueNum = read $ init $ head $ tail parts
        unMappedData = drop 2 parts

        aux :: Attrs -> [String] -> Attrs
        aux aM [] = aM
        aux aM (x:y:rst) = aux (M.insert x (read y) aM) rst
        aux _ _ = error "Bad data"

--             Master     Test
matchTarget :: SueData -> SueData -> Bool
matchTarget (SueData _ master) (SueData _ target) = aux master (M.assocs target)
    where
        aux :: Attrs -> [(String, Int)] -> Bool
        aux _ [] = True
        aux mAttr ((k, v): rst) = case M.lookup k mAttr of
            Nothing -> False
            Just mV -> if mV == v then True && (aux mAttr rst) else False


matchTarget2 :: SueData -> SueData -> Bool
matchTarget2 (SueData _ master) (SueData _ target) = aux master (M.assocs target)
    where
        aux :: Attrs -> [(String, Int)] -> Bool
        aux _ [] = True
        aux mAttr ((k, v): rst) = case M.lookup k mAttr of
            Nothing -> False
            Just mV -> case k of
                "cats"        -> if mV < v then True && (aux mAttr rst) else False
                "trees"       -> if mV < v then True && (aux mAttr rst) else False
                "pomeranians" -> if mV > v then True && (aux mAttr rst) else False
                "goldfish"    -> if mV > v then True && (aux mAttr rst) else False
                _             -> if mV == v then True && (aux mAttr rst) else False

