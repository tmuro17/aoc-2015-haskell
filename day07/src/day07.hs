import qualified Data.Text as T
import qualified Data.Text.IO as TIO


import Data.Bits
import Data.List.Split (splitOn)
import Safe
import Data.Maybe
import qualified Data.Map as M
import Data.Word

import Test.Tasty
import Test.Tasty.Hspec

data Prim = Num Word16 | Id String deriving (Show, Eq)

data Value = Assign Prim
           | And Prim Prim
           | Or Prim Prim
           | LShift Prim Int
           | RShift Prim Int
           | Not Prim
           deriving (Show, Eq)

data Wiring = Wire Value String deriving (Eq, Show)


main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day07" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input7.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 7a" $
                solvePart1 strs
                `shouldBe`
                Just 16076
            it "passes 7b" $
                solvePart2 strs
                `shouldBe`
                Just 2797


solvePart1 :: [String] -> Maybe Word16
solvePart1 = flip processWires M.empty . map toWiring

solvePart2 :: [String] -> Maybe Word16
-- The overwriting here is so jank, but I really don't care
solvePart2 = flip processWires (M.singleton "b" 16076) . filter (\(Wire v d) -> if (v == (Assign (Num 19138)) && d == "b") then False else True) . map toWiring

processWires :: [Wiring] -> WireMemory -> Maybe Word16
processWires wires wireMem = M.lookup "a" =<< aux wires wireMem
    where
        aux :: [Wiring] -> WireMemory -> Maybe WireMemory
        aux [] wm = Just wm
        aux [w] wm = processWire w wm
        aux (w:ws) wm = case processWire w wm of
            Nothing -> aux (ws ++ [w]) wm
            Just m -> aux ws m


type WireMemory = M.Map String Word16

processWire :: Wiring -> WireMemory -> Maybe WireMemory
processWire (Wire (Assign p) dst) wm = do
    pI <- numOrLookup p wm
    return $ M.insert dst pI wm

processWire (Wire (And a b) dst) wm = do
    aI <- numOrLookup a wm
    bI <- numOrLookup b wm
    return $ M.insert dst (aI .&. bI) wm

processWire (Wire (Or a b) dst) wm = do
    aI <- numOrLookup a wm
    bI <- numOrLookup b wm
    return $ M.insert dst (aI .|. bI) wm

processWire (Wire (LShift a n) dst) wm = do
    aI <- numOrLookup a wm
    return $ M.insert dst (shiftL aI n) wm

processWire (Wire (RShift a n) dst) wm = do
    aI <- numOrLookup a wm
    return $ M.insert dst (shiftR aI n) wm

processWire (Wire (Not a) dst) wm = do
    aI <- numOrLookup a wm
    return $ M.insert dst (complement aI) wm


numOrLookup :: Prim -> WireMemory -> Maybe Word16
numOrLookup (Num i) _ = Just i
numOrLookup (Id s) wm = M.lookup s wm

toWiring :: String -> Wiring
toWiring str = Wire (toValue src) dst
    where
        parts = splitOn " -> " str
        src = head parts
        dst = last parts


toValue :: String -> Value
toValue str = case toMaybeNum str of
    Just x -> Assign $ Num x
    Nothing -> toComplexVal str
    where
        toMaybeNum :: String -> Maybe Word16
        toMaybeNum s = readMay s

        toComplexVal :: String -> Value
        toComplexVal s | isJust first && isJust mid && fromJust first == "NOT" = Not $ toPrim $ fromJust mid
                       | isJust mid && isJust thrd && fromJust mid == "AND" = And (toPrim $ fromJust first) (toPrim $ fromJust thrd)
                       | isJust mid && isJust thrd && fromJust mid == "OR" = Or (toPrim $ fromJust first) (toPrim $ fromJust thrd)
                       | isJust mid && isJust thrd && fromJust mid == "LSHIFT" = LShift (toPrim $ fromJust first) (read $ fromJust thrd)
                       | isJust mid && isJust thrd && fromJust mid == "RSHIFT" = RShift (toPrim $ fromJust first) (read $ fromJust thrd)
                       | otherwise = Assign $ toPrim $ fromJust first
            where
                wds = words s
                first = headMay wds
                mid = headMay =<< tailMay wds
                thrd = lastMay wds


        toPrim :: String -> Prim
        toPrim s = case toMaybeNum s of
            Just x -> Num x
            Nothing -> Id s
