module UndirectedGraph
(G(..)
,GM(..)
,num_v
,edge
,neighbours
,allG
,allV
,connected
,degree
,maxDeg
,regular
)where

import qualified Data.Set as Set

type GM = [Int]
data G = G [[Bool]] deriving (Show, Eq)

num_v :: G -> Int
num_v (G e) = length e + 1

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

maxDeg :: G -> [Int] -> Int
maxDeg g gm = maximum [degree g gm x | x <- gm]

regular :: Int -> G -> Bool
regular k g@(G e) = all ((== k) . length) (map (neighbours g) [0 .. num_v g - 1])
