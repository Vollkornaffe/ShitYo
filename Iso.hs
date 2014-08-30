import UndirectedGraph
import Data.List
import Data.Char

data Id = Id [Char] deriving (Eq,Ord,Show)
data C = C Id GM deriving Show

--lableV :: G -> [C]

splitOnDeg :: G -> C -> [C]
splitOnDeg g (C (Id id) gm) = filter (\(C _ z) -> z /= [])
                         [(C (Id ((if (allK k == gm)
                                    then 'r'
                                    else intToDigit k)
                                   :id))
                             (allK k)) | k <- [0..maxDeg g gm]]
    where allK k' = [x | x <- gm,k'== (degree g gm x)]

sortId :: [Id] -> [Id]
sortId [] = []
sortId ids = (Id nextMax) : (sortId (delete (Id nextMax) ids))
    where nextMax = mirror (foldl
                                (\acu (Id xs) -> maxId (mirror xs []) acu) 
                                 "#"
                                 ids) []

mirror :: [Char] -> [Char] -> [Char]
mirror [] ys = ys
mirror (x:xs) (ys) = (mirror xs (x:ys))

maxId :: [Char] -> [Char] -> [Char]
maxId [] ys = ys
maxId xs [] = xs
maxId (x:xs) (y:ys) = case (compare x y) of
                         EQ -> x:(maxId xs ys)
                         GT -> (x:xs)
                         LT -> (y:ys)

