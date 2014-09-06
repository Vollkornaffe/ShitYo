import UndirectedGraph
import Data.List
import Data.Char
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

type Id = [Char]
type C = (Id, GM)
type Cs = Map Id GM

splitOnDeg :: G -> C -> Cs
splitOnDeg g (id, gm) = M.fromList [((if allK k == gm
                                      then "r"
                                      else show k ++ ".") ++ id,
                                     allK k) | k <- [0..maxDeg g gm],
                                               allK k /= []]
    where allK k' = [x | x <- gm,k'== (degree g gm x)]

sortId :: [Id] -> [Id]
sortId [] = []
sortId ids = nextMax : (sortId (delete nextMax ids))
    where nextMax = mirror (foldl
                                (\acu xs -> maxId (mirror xs []) acu) 
                                 "#"
                                 ids) []

mirror :: [Char] -> [Char] -> [Char]
mirror [] ys = ys
mirror (x:xs) (ys) = (mirror xs (x:ys))

maxId :: [Char] -> [Char] -> [Char]
maxId [] ys = ys
maxId xs [] = xs
maxId (x:xs) (y:ys) = case compare x y of
                         EQ -> x:(maxId xs ys)
                         GT -> (x:xs)
                         LT -> (y:ys)

giveId :: Int -> Cs -> Id
giveId j = fromJust . M.foldrWithKey f Nothing
    where f k gm b | j `elem` gm = case b of
                                       Nothing -> Just k
                                       Just _ -> error "multiple matches"
                   | otherwise = b

neighbourIds :: G -> Int -> Cs -> [Id]
neighbourIds g j cs = sortId $ map (flip giveId cs) (neighbours g j)

update :: G -> Cs -> C -> Cs
update g cs (id, gm) = if sort gm == sort (head classes)
                       then M.fromList [('C':id, gm)]
                       else M.fromList $ zipWith 
			    		(\gm i -> (show i ++ "." ++ id, gm)) classes [0..]
    where cmp :: Int -> Int -> Ordering
          cmp i j = compare (concat (neighbourIds g i cs))
                            (concat (neighbourIds g j cs))

          sorted = sortBy cmp gm 

          f ((j:gm):gms) i = case cmp i j of
                                EQ -> (i:j:gm):gms
                                _ -> [i]:(j:gm):gms

          classes = foldl f [[head sorted]] (tail sorted)

ready :: Cs -> Bool
ready = M.foldrWithKey f True
    where f ('C':k) gm b = b && True
          f _ _ _ = False

splitRec :: G -> C -> Cs 
splitRec g (id, gm) = if sort gm == sort (head classes)
                      then M.fromList [('f':id, gm)]
                      else M.fromList $ zipWith
                        (\gm i -> (show i ++ "." ++ id, gm)) classes [0..]
    where cmp :: Int -> Int -> Ordering
          cmp i j = compare (result i) (result j)
          
          result k = concat [if gknubs k n == []
                             then "#" 
                             else concat $ M.keys (label 
                                                   (projectGraph g (gknubs k n)) 
                                                   (M.fromList[("#",gknubs k n)])) 
                            | n <- [0..length (vView g gm k) - 1]]

          gknubs k n = (knubs g (vView g gm k)) !! n
          sorted = sortBy cmp gm

          f ((j:gm):gms) i = case cmp i j of
                                EQ -> (i:j:gm):gms
                                _ -> [i]:(j:gm):gms
          classes = foldl f [[head sorted]] (tail sorted)

knubs :: G -> [GM] -> [GM]
knubs _ [] = []
knubs _ (x:[]) = x:[]
knubs g (x:xs) = [a|a<-x,not $ any (edge g a) (concat xs)] : (knubs g xs)

vView :: G -> GM -> Int -> [GM]
vView g gm n = filterLayers [] ([n]:(f [n]))
    where f xs = if (xs == gm)
                 then []
                 else a : (f a)
                    where a = [x|x<-gm,any (edge g x) xs]

filterLayers :: GM -> [GM] -> [GM]
filterLayers _ [] = []
filterLayers ban (x:xs) = fEd : (filterLayers (ban ++ fEd) xs)
    where fEd = [a | a<-x, not (a `elem` ban)]

projectGraph :: G -> GM -> G
projectGraph g gm = G (tail $ listToMatrix newE)
    where newV = [1 .. (length gm)]
          newE = [(0,z) | z <- [1 .. (length gm)]] ++ [(x,y) | x<-newV,y<-newV,edge g (gm !! x) (gm !! y)]



listToMatrix :: [(Int, Int)] -> [[Bool]]
listToMatrix es = let n = maximum (map fst es ++ map snd es)
                      entry i j = (i,j) `elem` es ||
                                  (j,i) `elem` es
                      row i = map (entry i) [(i+1) .. n]
                  in map row [0 .. n-1]

--data G' = G' GM G deriving Show 
--projectGraph :: G' -> GM -> G'
--projectGraph (G' gm g) newGm = G' newGm $ G (listToMatrix newE)
--    where a x = elemIndex (newGm !! x) gm 
--          e Nothing _ = False
--          e _ Nothing = False
--          e (Just x) (Just y) = edge g x y
--          newV = [0 .. (length newGm)-1]
--          newE = [(x,y) | x<-newV,y<-newV,e (a x) (a y)]

label :: G -> Cs -> Cs
label _ [] = error "no inital Class"
label g cs = map someAction cs
    where someAction 

testG = G [[True,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[True,False,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[False,False,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,True,False,False,False,False,False,True,False,False,False,False,False,True],[True,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[True,False,True,False,False,False,False,False,False,False,False,False,False,False,False,False],[False,False,True,False,False,False,False,False,False,False,False,False,False,False,False],[True,True,False,False,False,False,False,False,False,False,False,False,False,False],[True,False,False,False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,True,False,False,False,False,False,True],[True,True,True,False,False,False,False,False,False,False,False],[True,False,True,False,False,False,False,False,False,False],[False,False,True,False,False,False,False,False,False],[True,True,False,False,False,False,False,False],[True,False,False,False,False,False,False],[False,False,False,False,False,True],[True,True,True,False,False],[True,False,True,False],[False,False,True],[True,True],[True]]


