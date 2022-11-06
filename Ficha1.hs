{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ficha1 where
import Data.Char hiding (isLower)

--Exercício 1.a

perimetro :: Float -> Float
perimetro raio = 2*pi*raio

--Exercício 1.b

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((y2 - y1)^2 + (x2 - x1)^2)

--Exercício 1.c

primUlt :: [a] -> (a,a)
primUlt (head:tail) = (head, last tail)

--Exercício 1.d

multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

--Exercício 1.e

truncaImpar :: [a] -> [a]
truncaImpar lista = if even (length lista)
                        then lista
                            else tail lista

--Exercício 1.f

max2 :: Int -> Int -> Int
max2 a b = if a>b
                then a
                    else b

--Exercício 1.g

max3 :: Int -> Int -> Int
max3 max2 c = if max2>c
                    then max2
                        else c

--Exercício 2.a

nRaizes :: Float -> Float -> Float -> Float
nRaizes a b c = let delta = (b^2-(4*a*c))
                in if delta > 0
                   then 2
                        else if delta == 0
                        then 1
                        else 0

--Exercício 2.b

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = (((-b) + sqrt(b^2 - 4*a*c)) / (2 * a), ((-b) - sqrt(b^2 - 4*a*c)) / (2 * a))

--Exercício 3
type Hora = (Int,Int)
--Exercício 3.a

validar :: Hora -> Bool
validar (h,m) = h >= 0 && h < 24 && m >= 0 && m < 60

--Exercício 3.b

testar :: Hora -> Hora -> Bool
testar (h1,m1) (h2,m2) | h1 > h2 = True
                       | h1 == h2 && m1 > m2 = True
                       | otherwise = False

--Exercício 3.c

horas2min :: Hora -> Int
horas2min (h,m) = h * 60 + m

--Exercício 3.d

min2horas :: Int -> Hora
min2horas n = (div n 60, mod n 60)

--Exercício 3.e

calcular :: Hora -> Hora -> Int
calcular (h1,m1) (h2,m2) | testar (h1,m1) (h2,m2) == False = horas2min (h2 - h1, m2 - m1)
                         | otherwise = horas2min (h1 - h2,m1 - m2)
--Exercício 3.f

adicionar :: Hora -> Int -> Hora
adicionar h n = min2horas(horas2min h + n)

--Exercício 4
data HoraDois = H Int Int deriving (Show, Eq)
--Exercício 4.a

validarHora :: HoraDois -> Bool
validarHora (H h m) = h >= 0 && h < 24 && m >= 0 && m < 60

--Exercício 4.b

testarHora :: HoraDois -> HoraDois -> Bool
testarHora (H h1 m1) (H h2 m2) | h1 > h2 = True
                               | h1 == h2 && m1 > m2 = True
                               | otherwise = False

--Exercício 5
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)
--Exercício 5.a

next :: Semaforo -> Semaforo
next cor | cor == Verde = Amarelo
         | cor == Amarelo = Vermelho
         | cor == Vermelho = Verde

--Exercício 5.b

stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

--Exercício 5.c

safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = s1 == Vermelho || s2 == Vermelho

--Exercício 6
data Ponto = Cartesiano Double Double | Polar Double Double -- No Polar, o primeiro Double = raio // o segundo Double = amplitude em rads (que pode ser calculado a partir do sen/cos/tg e dos catetos)
             deriving (Show,Eq)
--Exercício 6.a

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar raio alfa) = if alfa == pi/2
                         then 0
                         else raio * cos alfa

--Exercício 6.b

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar raio alfa) = if alfa == pi
                         then 0
                         else raio * sin alfa

--Exercício 6.c

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2 + y^2)
raio (Polar raio alfa) = raio

--Exercício 6.d

angulo:: Ponto -> Double
angulo (Cartesiano x y) | x == 0 && y == 0 = 0
                        | x > 0 && y == 0 = 0
                        | x < 0 && y == 0 = pi
                        | x == 0 && y > 0 = pi/2
                        | x == 0 && y < 0 = 3 * pi/2
angulo (Polar raio alfa) = alfa

--Exercício 6.e

distancia :: Ponto -> Ponto -> Double
distancia (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)
distancia (Polar raio1 alfa1) (Polar raio2 alfa2) =
    sqrt(((raio2 * cos alfa2) - (raio1 * cos alfa1))^2 + ((raio2 * sin alfa2) - (raio1 * sin alfa1))^2)

--Exercício 7
data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)
--Exercício 7.a

poligono :: Figura -> Bool
poligono (Circulo ponto raio) = False
poligono (Rectangulo ponto1 ponto2) = posx ponto1 /= posx ponto2 || posy ponto1 /= posy ponto2
poligono (Triangulo p1 p2 p3) = (posy p2 - posy p1)/(posx p2 - posx p1) /= (posy p3 - posy p2)/(posx p3 - posx p2)

--Exercício 7.b

vertices :: Figura -> [Ponto]
vertices (Circulo ponto raio) = []
vertices (Rectangulo p1 p2) = [p1, Cartesiano (posx p1)(posy p2), Cartesiano (posx p2)(posy p2)]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]

--Exercício 7.c

area:: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = distancia p1 p2
        b = distancia p2 p3
        c = distancia p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- Formula de Heron
area (Circulo ponto raio) = pi*raio^2
area (Rectangulo p1 p2) = abs(posx p1 - posx p2) * abs(posy p1 - posy p2)

--Exercício 7.d
perimetroSete :: Figura -> Double
perimetroSete (Circulo ponto raio) = 2*pi*raio
perimetroSete (Rectangulo p1 p2) = abs(posx p1 - posx p2)*2 + abs(posy p1 - posy p2)*2
perimetroSete (Triangulo p1 p2 p3) = distancia p1 p2 + distancia p2 p3 + distancia p1 p3

--Exercício 8.a

isLower :: Char -> Bool
isLower c = ord c >= ord 'a' && ord c <= ord 'z'

--Exercício 8.b

isDigit :: Char -> Bool
isDigit d = ord d >= ord '0' && ord d <= ord '9'

--Exercício 8.c

isAlpha :: Char -> Bool
isAlpha c = isLower c || isUpper c
    where
        isUpper :: Char -> Bool
        isUpper c = ord c >= ord 'A' && ord c <= ord 'Z'

--Exercício 8.d

toUpper :: Char -> Char
toUpper c = if isLower c then chr ( ord c - 32) else c

-- Exercício 8.e

digitToInt :: Char -> Int
digitToInt c = ord c - 48