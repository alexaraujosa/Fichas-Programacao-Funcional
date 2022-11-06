{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ficha2 where
import Data.Char (ord)
--Exercício 2.a

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h*2 : dobros t

--Exercício 2.b

numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre c (x:xs) = if c == x then 1 + numOcorre c xs else numOcorre c xs

--Exercício 2.c

positivos :: [Int] -> Bool
positivos [n] = n >= 0
positivos (h:t) = if h < 0 then False else positivos t

--Exercício 2.d

soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) = if x < 0 then soPos xs else x : soPos xs

--Exercício 2.e

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if h > 0 then somaNeg t else h + somaNeg t

--Exercício 2.f

tresUlt :: [a] -> [a]
tresUlt l
  | length l <= 3 = l
  | length l > 3 = tresUlt (tail l)

--Exercício 2.g

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((p,s):t) = s: segundos t

--Exercício 2.h

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros c ((p,s):t) = if c == p then True else nosPrimeiros c t

--Exercício 2.i
--Porque nao pode ser sumTriplos ((x,y,z):xs) = (x + sumTriplos xs, y + sumTriplos xs, z + sumTriplos xs)
--Em vez de estar a escrever o where
--Porque nao pode ser sumTriplos [] = []

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [(x,y,z)] = (x,y,z)
sumTriplos ((x,y,z):xs) = (x + sumX, y + sumY, z + sumZ)
            where (sumX, sumY, sumZ) = sumTriplos xs

--Exercício 3.a

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if h `elem` ['0'..'9'] then h: soDigitos t else soDigitos t

--Exercício 3.b

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) = if x `elem` ['a'..'z'] then 1 + minusculas xs else minusculas xs

--Exercício 3.c

nums :: String -> [Int]
nums [] = []
nums (h:t) = if h `elem` ['0'..'9'] then (ord h - ord '0'):nums t else nums t

--Exercício 4

type Polinomio = [Monomio]
type Monomio = (Float,Int)
--Exercício 4.a

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((c,g):xs) = if n == g then 1 + conta n xs else conta n xs

--Exercício 4.b

grau :: Polinomio -> Int
grau [(c,g)] = g
grau ((c,g):t) = if g > grau t then g else grau t

--Exercício 4.c

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau grau ((c,g):xs) = if grau == g then (c,g):selgrau grau xs else selgrau grau xs

--Exercício 4.d

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,g):t) = if g == 0 then deriv t else (c * fromIntegral g, g - 1): deriv t

--Exercício 4.e

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula v ((c,g):t) = c* (v^g) + calcula v t

--Exercício 4.f

simp :: Polinomio -> Polinomio
simp [] = []
simp ((c,g):t) = if c == 0 then simp t else (c,g):simp t

--Exercício 4.g

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (c1,g1) ((c2,g2):t) = (c1 * c2, g1 + g2): mult (c1,g1) t

--Exercício 4.h
--Nao percebi perfeitamente, tenho de ver melhor depois a parte do Aux then (cm + gp, gm):xs porque é xs e nao normalizaAux xs
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,g):xs) = normalizaAux (c,g) (normaliza xs)

normalizaAux :: Monomio -> Polinomio -> Polinomio
normalizaAux m [] = [m]
normalizaAux (cm,gm) ((cp,gp):xs) = if gm == gp then (cm + cp, gm):xs else (cp,gp): normalizaAux (cm,gm) xs

--Exercício 4.i

soma :: Polinomio -> Polinomio -> Polinomio
soma p [] = p
soma [] p = p
soma ((c,g):t) p = somaAux (c,g) (soma t p)

somaAux :: Monomio -> Polinomio -> Polinomio
somaAux m [] = [m]
somaAux (cm,gm) ((cp,gp):t) = if gm == gp then (cm + cp, gm):t else (cp,gp):somaAux (cm,gm) t

--Exercício 4.j

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto ((cm,gm):xs) ((cp,gp):t) = (cm * cp, gm + gp):produto xs ((cp,gp):t)

--Exercício 4.k

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (m:t) = insere m (ordena t)

insere :: Monomio -> Polinomio -> Polinomio
insere m [] = [m]
insere (cm,gm) ((cp,gp):t) = if gp > gm then (cm,gm):(cp,gp):t else (cp,gp): insere (cm,gm) t

--Exercício 4.l

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)