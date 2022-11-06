{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module QuestoesAgain where
import Data.List (delete)
import Data.IntMap hiding (map, delete)

eenumFromTo :: Int -> Int -> [Int]
eenumFromTo x y | x == y = [x]
                | x <= y = x : eenumFromTo (x+1) y
                | x > y = [] 

eenumFromThenTo :: Int -> Int -> Int -> [Int]
eenumFromThenTo x y z | x < z && y - x > 0 || x > z && y - x < 0 = x : eenumFromThenTo y (2*y - x) z
                      | otherwise = []

maismais :: [a] -> [a] -> [a]
maismais [] l = l
maismais l [] = l
maismais (h:t) (x:xs) = h : maismais t (x:xs)

exclamacao :: [a] -> Int -> a
exclamacao (h:t) n = if n == 0 then h else exclamacao t (n-1)

rreverse :: [a] -> [a]
rreverse [] = []
rreverse (h:[]) = [h]
rreverse (h:t) = last t : rreverse (h:init (t))

ttake :: Int -> [a] -> [a]
ttake 0 l = []
ttake n [] = []
ttake n l@(h:t) = if n >= length l then l else h : ttake (n-1) t

ddrop :: Int -> [a] -> [a]
ddrop 0 l = l
ddrop n [] = []
ddrop n l@(h:t) = if n >= length l then [] else ddrop (n-1) t

zzip :: [a] -> [b] -> [(a,b)]
zzip l [] = []
zzip [] l = []
zzip (h:t) (x:xs) = (h,x) : zzip t xs

rreplicate :: Int -> a -> [a]
rreplicate 0 l = []
rreplicate n c | n > 0 = c : rreplicate (n-1) c
               | otherwise = []

iintersperce :: a -> [a] -> [a]
iintersperce n [] = []
iintersperce n [x] = [x]
iintersperce n (h:t) = h : n : iintersperce n t

-- Primeiro, definir uma função que, de uma lista, crie outra apartir dos primeiros elementos que são iguais
-- Segundo, já na ggroup, quero a listagem dos primeiros elementos que são iguais e, depois, como segundo elemento da lista principal,
-- uma nova lista criada com os segundos elementos iguais, ou seja, a listagem da lista principal sem os X elementos que já estão 
-- na lista que se encontra em primeiro elemento da principal, que se obtem através da drop (do tamanho da lista que se encontra em primeiro elemento)
-- com, o segundo argumento, a lista principal, porque o drop exige dois argumentos, neste caso, um inteiro, que é o tamanho da lista em 
-- primeiro elemento e uma lista, nesta caso, a lista principal.
ggroup :: Eq a => [a] -> [[a]]
ggroup [] = []
ggroup l = listagem l : ggroup (drop (length (listagem l)) l)

listagem :: Eq a => [a] -> [a]
listagem [x] = [x]
listagem (h:t) = if h == head t then h:listagem (h:drop 1 t) else [h]

cconcat :: [[a]] -> [a]
cconcat [] = []
cconcat (h:t) = h ++ cconcat t

-- Decorar
iinits :: [a] -> [[a]]
iinits [] = [[]]
iinits l = iinits (init l) ++ [l]

ttails :: [a] -> [[a]]
ttails [] = [[]]
ttails l = [l] ++ ttails (tail l)

hheads :: [[a]] -> [a]
hheads [] = []
hheads ([]:t) = hheads t
hheads ((h1:t1):t) = h1 : hheads t
        
ttotal :: [[a]] -> Int
ttotal [] = 0
ttotal (h:t) = length h + ttotal t

ffun :: [(a,b,c)] -> [(a,c)]
ffun [] = []
ffun ((a1,b1,c1):t) = (a1,c1) : ffun t

ccola :: [(String,b,c)] -> String
ccola [] = []
ccola ((a1,b1,c1):t) = a1 ++ ccola t

iidade :: Int -> Int -> [(String,Int)] -> [String]
iidade _ _ [] = []
iidade ano id ((a,b):t) | ano - b >= id = a: iidade ano id t
                        | otherwise = iidade ano id t
-- Decorar
ppowerEnumFrom :: Int -> Int -> [Int]
ppowerEnumFrom n 1 = [1]
ppowerEnumFrom n m | m > 1 = ppowerEnumFrom n (m - 1) ++ [n^(m-1)]
                   | otherwise = []

iisPrime :: Int -> Bool
iisPrime n | n >= 2 = iisPrimeAux n 2

            where 
                iisPrimeAux :: Int -> Int -> Bool
                iisPrimeAux n m | m * m > n = True
                                | mod n m == 0 = False
                                | otherwise = iisPrimeAux n (m+1)

iisPrefixOf :: Eq a => [a] -> [a] -> Bool
iisPrefixOf l [] = False
iisPrefixOf [] l = True
iisPrefixOf (h1:t1) (h2:t2) = h1 == h2 && iisPrefixOf t1 t2

iisSuffixOf :: Eq a => [a] -> [a] -> Bool
iisSuffixOf l [] = False
iisSuffixOf [] l = True
iisSuffixOf l l2@(h:t) = l == l2 || iisSuffixOf l t

iisSubsequenceOf :: Eq a => [a] -> [a] -> Bool
iisSubsequenceOf [] l = True
iisSubsequenceOf l [] = False
iisSubsequenceOf (x:xs) (h:t) = x == h && iisSubsequenceOf xs t || iisSubsequenceOf (x:xs) t

eelemIndices :: Eq a => a -> [a] -> [Int]
eelemIndices n [] = []
eelemIndices n (h:t) = if n == h then 0 : map (+1) (eelemIndices n t) else map (+1) (eelemIndices n t)

nnub :: Eq a => [a] -> [a]
nnub [] = []
nnub (h:t) = if h `elem` t then nnub t else h : nnub t

ddelete :: Eq a => a -> [a] -> [a]
ddelete n [] = []
ddelete n (h:t) = if n == h then t else h : ddelete n t

barras :: Eq a => [a] -> [a] -> [a]
barras l [] = []
barras [] l = l
barras l (h:t) = barras (delete h l) t

uunion :: Eq a => [a] -> [a] -> [a]
uunion l [] = l
uunion [] l = l
uunion l (h:t) = if h `elem` l then uunion l t else uunion (l ++ [h]) t

iintersect :: Eq a => [a] -> [a] -> [a]
iintersect [] l = []
iintersect (h:t) l | h `elem` l = h : iintersect t l
                   | otherwise = iintersect t l

iinsert :: Ord a => a -> [a] -> [a]
iinsert n [] = [n]
iinsert n l@(h:t) | n <= h = n : l
                  | otherwise = h : iinsert n t

uunwords :: [String] -> String
uunwords [] = ""
uunwords (h:t) = h ++ (if t == [] then "" else " ") ++ uunwords t

uunlines :: [String] -> String
uunlines [] = []
uunlines (h:t) = h ++ "\n" ++ uunlines t

ppMaior :: Ord a => [a] -> Int
ppMaior [x] = 0
ppMaior (h:t) = if h >= (t !! ppMaior t) then 0 else 1 + ppMaior t

llookup :: Eq a => a -> [(a,b)] -> Maybe b
llookup c [] = Nothing
llookup c ((a,b):t) | c == a = Just b
                    | otherwise = llookup c t

ppreCrescente :: Ord a => [a] -> [a]
ppreCrescente [] = []
ppreCrescente [x] = [x]
ppreCrescente (x:y:z) = if x < y then y : ppreCrescente (y:z) else [x]

iiSort :: Ord a => [a] -> [a]
iiSort [] = []
iiSort (h:t) = insert h (iiSort t)

mmenor :: String -> String -> Bool
mmenor p [] = False
mmenor [] p = True
mmenor (h:t) (x:xs) | h < x = True
                    | h == x = mmenor t xs
                    | otherwise = False

eelemMSet :: Eq a => a -> [(a,Int)] -> Bool
eelemMSet n [] = False
eelemMSet n ((a,v):t) = n == a || eelemMSet n t

cconverteMSet :: [(a,Int)] -> [a]
cconverteMSet [] = []
cconverteMSet ((a,1):t) = a : cconverteMSet t
cconverteMSet ((a,v):t) = a : cconverteMSet ((a, v-1):t)

iinsereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
iinsereMSet c [] = [(c,1)]
iinsereMSet c ((a,v):t) = if c == a then ((a,v+1):t) else (a,v) : iinsereMSet c t

rremoveMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
rremoveMSet c [] = []
rremoveMSet c ((a,v):t) | c == a = if v > 1 then (a, v-1):t else t
                        | otherwise = (a,v) : rremoveMSet c t

cconstroiMSet :: Ord a => [a] -> [(a,Int)]
cconstroiMSet [] = []
cconstroiMSet (h:t) = iinsereMSet h (cconstroiMSet t)

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):t) = (a : as,bs)
    where (as,bs) = partitionEithers t
partitionEithers ((Right b):t) = (as,b : bs)
    where (as,bs) = partitionEithers t

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of Nothing -> catMaybes t
                            Just a -> a : catMaybes t

data Movimento = Norte | Sul | Este | Oeste
    deriving Show

caminho :: (Int, Int) -> (Int, Int) -> [Movimento]
caminho (xi, yi) (xf, yf) 
    | xi < xf = Este : caminho (xi + 1, yi) (xf, yf)
    | xi > xf = Oeste : caminho (xi - 1, yi) (xf, yf)
    | yi < yf = Norte : caminho (xi, yi + 1) (xf, yf)
    | yi > yf = Sul : caminho (xi, yi - 1) (xf, yf)
    | otherwise = []

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x, y) (Norte:t) = posicao (x, y + 1) t
posicao (x, y) (Sul:t) = posicao (x, y - 1) t
posicao (x, y) (Este:t) = posicao (x + 1, y) t
posicao (x, y) (Oeste:t) = posicao (x - 1, y) t

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops posi movs = posi == posicao posi movs || hasLoops posi (init movs)

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (h:t) 
    | eQuadrado h = 1 + contaQuadrados t
    | otherwise = contaQuadrados t

eQuadrado :: Rectangulo -> Bool
eQuadrado (Rect (x1,y1) (x2,y2)) = abs (y2 - y1) == abs (x2 - x1)

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs (x2 - x1) * abs (y2 - y1) + areaTotal t

data Equipamento = Bom | Razoavel | Avariado
    deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (h:t) = case h of Bom -> 1 + naoReparar t
                             Razoavel -> 1 + naoReparar t
                             Avariado -> naoReparar t