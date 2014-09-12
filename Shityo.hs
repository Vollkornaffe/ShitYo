import Data.List ((!!), delete, partition, sort, nub, elemIndex)
import Debug.Trace
import qualified Data.Set as Set 
import Data.Char (intToDigit)

import Debug.Trace
import UndirectedGraph

splitOffFirstGroup :: (a -> a -> Bool) -> [a] -> ([a],[a])
splitOffFirstGroup equal xs@(x:_) = partition (equal x) xs
splitOffFirstGroup _     []       = ([],[])

equivalenceClasses _     [] = []
equivalenceClasses equal xs = let (fg,rst) = splitOffFirstGroup equal xs
                              in fg : equivalenceClasses equal rst
                              
analyse :: (G -> Bool) -> Int -> String
analyse f n = let gs = filter f (allG n)
                  cgs = filter connected gs
                  cs = equivalenceClasses iso gs
                  ccs = equivalenceClasses iso cgs
              in "n=" ++ show n ++ ": "
                 ++ show (length gs) ++ " graphs, "
                 ++ show (length cgs) ++ " connected graphs (" ++ show (((fromIntegral (length cgs)) :: Float) / (fromIntegral (length gs))) ++ "), "
                 ++ show (length cs) ++ " classes, "
                 ++ show (length ccs) ++ " connected classes ("  ++ show (((fromIntegral (length ccs)) :: Float) / (fromIntegral (length cs))) ++ "), "

type ID = [Char]
data O1Cl = Reg ID GM | O1cl ID GM [O1Cl] 
--data O2Cl = O2cl [(Layer,GM)]

instance Show O1Cl where
    show (Reg id gm)        =  "   " ++ "R:" ++ show id ++ " : "  ++ show gm ++ "\n"
    show (O1cl id gm xs)    =  show id ++ " : " ++ show gm ++ "\n" ++ concat (map show xs)

splitOnDeg :: G -> O1Cl -> O1Cl
splitOnDeg _ reg@(Reg _ _)      = reg
splitOnDeg g cl@(O1cl id gm _)  = O1cl id gm (map (splitOnDeg g) (markReg(ord1 g cl)))
    where markReg = map (\a@(O1cl id' gm' _) -> if (last id == last id') then Reg id' gm'
                                                                         else a)
                               
ord1 :: G -> O1Cl -> [O1Cl]
ord1 g (O1cl oldId gm _) = [O1cl (oldId ++ ['.',(intToDigit k)]) (allVK k) [] 
                           | k <- [0..maxDeg g gm],(allVK k) /= []]
    where allVK k' = [x | x <- gm,k'== (degree g gm x)]

   
readGraph :: FilePath -> IO [[Bool]]
readGraph f = parseGraph `fmap` readFile f

step :: Maybe Int -> [String] -> [(Int, Int)]
step Nothing ("source":i:ws) = step (Just $ read i) ws
step (Just i) ("target":j:ws) = (i, read j):(step Nothing ws)
step Nothing (_:ws) = step Nothing ws
step Nothing [] = []

listToMatrix :: [(Int, Int)] -> [[Bool]]
listToMatrix es = let n = maximum (map fst es ++ map snd es)
                      entry i j = (i,j) `elem` es ||
                                  (j,i) `elem` es
                      row i = map (entry i) [(i+1) .. n]
                  in map row [0 .. n-1]

parseGraph :: String -> [[Bool]]
parseGraph s = listToMatrix $ step Nothing (words s)


main = do
    test <- (readGraph "test.gml")
    --testG = G test
    putStrLn "Graph succ loaded..."



