import UndirectedGraph
import Data.Char

type Id = [Char]
data C = C Id GM deriving Show

--lableV :: G -> [C]

splitOnDeg :: G -> C -> [C]
splitOnDeg g (C id gm) = filter (\(C _ z) -> z /= [])
                         [(C ((if (allK k == gm)
                               then 'r'
                               else intToDigit k):id) (allK k))
                         | k <- [0..maxDeg g gm]]
    where allK k' = [x | x <- gm,k'== (degree g gm x)]
