import UndirectedGraph
import Data.List
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe
import Debug.Trace

type Id = [Char]
type C = (Id, GM)
type Cs = Map Id GM

splitOnDeg :: G -> C -> Cs
splitOnDeg g (id, gm) = M.fromList [((if allK k == gm
                                      then "r" ++ show k ++ ":" ++ show (length (allK k)) ++ "."
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
                       then M.fromList [('C':'.':id, gm)]
                       else M.fromList $ zipWith 
                        (\gm i -> (show i ++ "/" ++ show (length gm) ++ "." ++ id, gm)) classes [0..]
    where cmp :: Int -> Int -> Ordering
          cmp i j = compare (concat (neighbourIds g i cs))
                            (concat (neighbourIds g j cs))

          sorted = sortBy cmp gm 

          f ((j:gm):gms) i = case cmp i j of
                                EQ -> (i:j:gm):gms
                                _ -> [i]:(j:gm):gms

          classes = foldl f [[head sorted]] (tail sorted)

splitRec :: G -> C -> Cs 
splitRec g (id, gm) = if sort gm == sort (head classes)
                      then splitRec' g ('f':id, gm)
                      else M.fromList $ zipWith
                        (\gm i -> (show i ++ "." ++ id, gm)) classes [0..]
    where cmp :: Int -> Int -> Ordering
          cmp i j = compare (result i) (result j)
          
          result k = concat [if gknubs k n == []
                             then "#" 
                             else concat $ M.keys (label 
                                                   (projectGraph g (gknubs k n) [(a,b)|a<-(gknubs k n),b<-(gknubs k n)]) 
                                                   (M.fromList[("#",[0 .. length (gknubs k n) - 1])])) 
                            | n <- [0..length (vView g gm k) - 1]]

          gknubs k n = (knubs g (vView g gm k)) !! n
          sorted = sortBy cmp gm

          f ((j:gm):gms) i = case cmp i j of
                                EQ -> (i:j:gm):gms
                                _ -> [i]:(j:gm):gms
          classes = foldl f [[head sorted]] (tail sorted)

splitRec' :: G -> C -> Cs 
splitRec' g (id, gm) = if sort gm == sort (head classes)
                      then M.fromList [('F':id, gm)]
                      else M.fromList $ zipWith
                        (\gm i -> (show i ++ "." ++ id, gm)) classes [0..]
    where cmp :: Int -> Int -> Ordering
          cmp i j = compare (result i) (result j)
          
          result k = concat [if gknubs k n == []
                             then "#" 
                             else concat $ M.keys (label 
                                                   (projectGraph g (gknubs k n) [(a,b)|a<-(gknubs k n),b<-(gknubs k n),
                                                                                          (a `elem` ((vView g gm k)!!n))
                                                                                       /= (b `elem` ((vView g gm k)!!n))]) 
                                                   (M.fromList[("#",[0 .. length (gknubs k n) - 1])])) 
                            | n <- [0..length (vView g gm k) - 1]]

          gknubs k n = (zipWith (++) ((vView g gm k)++[]) ([]++(vView g gm k))) !! n
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
vView g gm n = [n]:(f [n] (Set.fromList [n]))
    where f [] _ = [] 
          f xs s = if (xs == gm)
                   then []
                   else a : (f a (foldr Set.insert s a))
              where a = [x|x<-gm,any (edge g x) xs, not (Set.member x s)]

ready :: Char -> Cs -> Bool
ready c = M.foldrWithKey f True
    where f (c':k) gm b = if c' == c then b && True else False

label :: G -> Cs -> Cs
label g cs | M.size cs == num_v g = prepend cs "completely sorted."
           | num_e g == 0 = prepend cs ("no edges:" ++ show (num_v g) ++ ".") 
           | complete g = prepend cs ("complete graph:" ++ show (num_v g) ++ ".")
           | connected g && regular 2 g = prepend cs ("2-regular and connected:" ++ show (num_v g) ++ ".")
           | otherwise = {-case (ready 'r' cs,ready 'C' cs,ready 'f' cs) of
                             (True,_,_) -> label g $ M.unions (map (\c -> (update g cs c)) (M.toAscList cs))
                             (_,True,_) -> label g $ M.unions (map (\c -> (splitRec g c)) (M.toAscList cs))
                             (_,_,True) -> cs
                             otherwise  -> label g $ M.unions (map (splitOnDeg g) (M.toAscList cs))-}
                         case (ready 'r' cs,ready 'C' cs,ready 'F' cs) of
                             (True,_,_) -> label g $ M.unions (map (\c -> trace ("update " ++ show g ++ " " ++ show cs) (update g cs c)) (M.toAscList cs))
                             (_,True,_) -> label g $ M.unions (map (\c -> trace ("split rec " ++ show c) (splitRec g c)) (M.toAscList cs))
                             (_,_,True) -> trace ("done for " ++ show cs) cs
                             otherwise  -> label g $ trace ("otherwise " ++ show cs) (M.unions (map (splitOnDeg g) (M.toAscList cs)))
    where prepend cs s = M.mapKeys (\id -> s ++ id) cs

testG = G [[True,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[True,False,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[False,False,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,True,False,False,False,False,False,True,False,False,False,False,False,True],[True,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[True,False,True,False,False,False,False,False,False,False,False,False,False,False,False,False],[False,False,True,False,False,False,False,False,False,False,False,False,False,False,False],[True,True,False,False,False,False,False,False,False,False,False,False,False,False],[True,False,False,False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,True,False,False,False,False,False,True],[True,True,True,False,False,False,False,False,False,False,False],[True,False,True,False,False,False,False,False,False,False],[False,False,True,False,False,False,False,False,False],[True,True,False,False,False,False,False,False],[True,False,False,False,False,False,False],[False,False,False,False,False,True],[True,True,True,False,False],[True,False,True,False],[False,False,True],[True,True],[True]]

testShit :: Map Id GM

testShit = M.fromList [("0.3.#",[18]),("2.3.#",[2,3,4,8,9,10,14,15,16]),("3.3.#",[0,1,6,7,12,13]),("r0.1.#",[21]),("r1.2.#",[19,20]),("r2.5.#",[5,11,17])]

canon :: G -> [Id]
canon g = map fst $ M.toAscList (label g (M.fromList [("#", [0..num_v g - 1])]))

goodIso :: G -> G -> Bool
goodIso a b = canon a == canon b

peterson :: G
peterson = G $ listToMatrix 10 [(0,1),(1,2),(2,3),(3,4),(4,0),
                                (0,5),(1,6),(2,7),(3,8),(4,9),
                                (5,7),(6,8),(7,9),(8,5),(9,6)]
