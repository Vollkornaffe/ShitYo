module UndirectedGraph
(G(..)
,GM(..)
,num_v
,num_e
,edge
,neighbours
,allG
,allV
,connected
,degree
,maxDeg
,complete
,regular
,iso
,bijections
,projectGraph
,reorder
,listToMatrix
,graphFromString
,testStr1
,testStr2
)where

import qualified Data.Set as Set
import Control.Monad.Random

import Data.List (delete)

type GM = [Int]
data G = G [[Bool]] deriving (Show, Eq)

num_v :: G -> Int
num_v (G e) = length e + 1

num_e :: G -> Int
num_e (G e) = foldl (\ a b -> a + (foldl (\ a' b' -> a' + (if b' then 1 else 0)) 0 b)) 0 e

edge :: G -> Int -> Int -> Bool
edge (G e) i j | i < j = (e !! i) !! (j-(i+1))
               | j < i = (e !! j) !! (i-(j+1))
               | True = False
               
neighbours :: G -> Int -> [Int]
neighbours g@(G e) i  = [j | j <- [0.. num_v g - 1], edge g i j]

allG :: Int -> [G]
allG n = map G (p (n-1))
    where p :: Int -> [[[Bool]]]
          p 0 = [[]]
          p k = concat $ map (\r -> map (:r) (ls k)) (p (k-1))
          
          ls :: Int -> [[Bool]]
          ls 0 = [[]]
          ls i = concat $ map (\l -> map (:l) [False, True]) (ls (i-1))

allV :: G -> GM
allV g = [0 .. num_v g - 1]

complete :: G -> Bool
complete (G g) = all (\row -> all id row) g

empty :: G -> Bool
empty (G g) = all (\row -> all not row) g 

--randomG :: RandomGen g => Int -> Rand g G
--randomG n | n == 1 = do l <- line 1
--                        return (G [l])
--          | otherwise = do G g <- randomG (n-1)
--                           l <- line n
--                           return (G (l:g))
--    where flip :: RandomGen g => Rand g Bool
--          flip = (==1) `fmap` getRandomR (1,2)
--          
--          line :: RandomGen g => Int -> Rand g [Bool]
--          line n = sequence (replicate n flip)
          
reachable :: G -> Int -> Set.Set Int -> Set.Set Int
reachable g i v = foldl (\v' j -> if not (Set.member j v')
                                  then Set.union v' (reachable g j v')
                                  else v')
                        (Set.insert i v)
                        (neighbours g i)

connected :: G -> Bool
connected g | num_v g == 0 = True
            | otherwise = Set.toAscList (reachable g 0 Set.empty) == [0 .. num_v g - 1]

degree :: G -> GM -> Int -> Int
degree g gm i = length [j | j <- (allV g),j `elem` gm,edge g i j]

maxDeg :: G -> GM -> Int
maxDeg g gm = maximum [degree g gm x | x <- gm]

regular :: Int -> G -> Bool
regular k g@(G e) = all ((== k) . length) (map (neighbours g) [0 .. num_v g - 1])

type Bijection = [Int]

bijections :: Int -> [Bijection]
bijections n = bs n [0..n-1]
    where  bs 0 _  = [[]]
           bs n xs = concat $ map (\x -> map (x:) (bs (n-1) (delete x xs))) xs

projectGraph :: G -> GM -> [(Int,Int)] -> G
projectGraph g gm allowedE = G $ listToMatrix (length gm) (filter (`elem` allowedE)newE)
    where newV = [0 .. (length gm) - 1]
          newE = [(x,y) | x <- newV, y <- newV, edge g (gm !! x) (gm !! y)]

listToMatrix :: Int -> [(Int, Int)] -> [[Bool]]
listToMatrix n es = let entry i j = (i,j) `elem` es ||
                                    (j,i) `elem` es
                        row i = map (entry i) [(i+1) .. n-1]
                    in map row [0 .. n-2]

reorder :: Bijection -> G -> G
reorder b g = G $ listToMatrix (num_v g) newE
    where newV = [0 .. num_v g - 1]
          newE = [(x,y) | x <- newV, y <- newV, edge g (b !! x) (b !! y)]
    
           
iso :: G -> G -> Bool
iso g1@(G e1) g2@(G e2) | num_v g1 /= num_v g2 = False
                        | otherwise = any c (bijections (num_v g1))
    where c :: Bijection -> Bool
          c f = all (\(a, b) -> (edge g1 a b) == (edge g2 (f !! a) (f !! b))) [(a, b) | a  <- [0..num_v g1 - 1], b <- [0..num_v g1 - 1]]
          
graphFromString :: String -> G
graphFromString s = G $ filter ((/= 0) . length) $ zipWith (\i l -> map (\x -> if x == '0' then False else True) (drop i l)) [1..] (lines s)

testStr1 = "010000000000000000000100000001\n101000001000000000000000000000\n010100000000000000000000010000\n001010000000100000000000000000\n000101000000000001000000000000\n000010100000000000000010000000\n000001010000000000000000000100\n000000101000001000000000000000\n010000010100000000000000000000\n000000001010000000100000000000\n000000000101000000000001000000\n000000000010100000000000000010\n000100000001010000000000000000\n000000000000101000001000000000\n000000010000010100000000000000\n000000000000001010000000100000\n000000000000000101000000000001\n000010000000000010100000000000\n000000000100000001010000000000\n000000000000000000101000001000\n000000000000010000010100000000\n100000000000000000001010000000\n000001000000000000000101000000\n000000000010000000000010100000\n000000000000000100000001010000\n001000000000000000000000101000\n000000000000000000010000010100\n000000100000000000000000001010\n000000000001000000000000000101\n100000000000000010000000000010"
testStr2 = "000000000000010001000000000100\n000010000000000000001000100000\n000000000010000000001001000000\n000000100100001000000000000000\n010000011000000000000000000000\n000000000001000010000000001000\n000100000000000010000000100000\n000010000000000000100010000000\n000010000001000100000000000000\n000100000000100100000000000000\n001000000000000000010100000000\n000001001000000000000000000100\n000000000100010000000000010000\n100000000000100000100000000000\n000100000000000000000011000000\n000000001100000000010000000000\n000001100000000000000100000000\n100000000000000000000000100001\n000000010000010000000100000000\n000000000010000100000000000001\n011000000000000000000000010000\n000000000010000010100000000000\n000000010000001000000000000010\n001000000000001000000000000100\n010000100000000001000000000000\n000000000000100000001000001000\n000001000000000000000000010010\n100000000001000000000001000000\n000000000000000000000010001001\n000000000000000001010000000010"
