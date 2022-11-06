{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ficha3 where

data Hora = H Int Int
            deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- Functions from Ficha1.hs

-- Verifica se um par de inteiros corresponde a uma hora do dia válida.
validarHora :: Hora -> Bool
validarHora (H h m) = h >= 0 && h < 24 && m >= 0 && m < 60
-- Verifica se uma hora é depois da outra.
nextHora :: Hora -> Hora -> Bool
nextHora (H h1 m1) (H h2 m2) = h1 < h2 || (h1 == h2 && m1 < m2)
-- Converte um valor em horas (par de inteiros) em minutos (inteiro).
horas2min :: Hora -> Int
horas2min (H h m) = 60 * h + m
-- Converte um valor em minutos (Inteiro) em horas (par de inteiros).
min2horas :: Int -> Hora
min2horas minutos = H (div minutos 60) (mod minutos 60)
-- Calcula a diferenca entre duas Horas, sendo o resultado em minutos.
hourMinusHour :: Hora -> Hora -> Int
hourMinusHour (H h1 m1) (H h2 m2) | not (nextHora (H h1 m1) (H h2 m2)) = horas2min (H (h2 - h1) (m2 - m1))
                                  | otherwise = horas2min (H (h1 - h2) (m1 - m2))
-- Adicionar 'n' minutos a uma determinada hora.
horaPlusmin :: Hora -> Int -> Hora
horaPlusmin (H h1 m1) min = min2horas (horas2min (H h1 m1) + min)

-- Exercício 1.a

verificarEtapa :: Etapa -> Bool
verificarEtapa (H h1 m1, H h2 m2) = validarHora (H h1 m1) && validarHora (H h2 m2) && nextHora (H h1 m1) (H h2 m2)

-- Exercício 1.b 

verificarViagem :: Viagem -> Bool
verificarViagem [] = True
verificarViagem [x] = verificarEtapa x
verificarViagem (h:t) = verificarEtapa h && nextHora (fst (head t)) (snd h) && verificarViagem t

-- Exercício 1.c

calcularHora :: Viagem -> Etapa
calcularHora (h:t) = (fst h, snd (last t)) 

-- Exercício 1.d

tempoViagem :: Viagem -> Int
tempoViagem [] = 0
tempoViagem (h:t) | verificarEtapa h = horas2min(snd h) - horas2min(fst h) + tempoViagem t
                  | otherwise = error "Viagem inválida."

-- Exercício 1.e

tempoEspera :: Viagem -> Int
tempoEspera [] = 0
tempoEspera [(x,xs)] = 0
tempoEspera [h,t] = horas2min (fst t) - horas2min (snd h)
tempoEspera l@(h:t) | verificarViagem l = horas2min (fst (head t)) - horas2min (snd h) + tempoEspera t -- l@(h:t) Serve para chamar a lista de 'l'
                    | otherwise = error "Viagem inválida."                                             -- ou de (h:t), sem ter que definir novamente.

-- Exercício 1.f

tempoTotal :: Viagem -> Int
tempoTotal [] = 0
tempoTotal l = tempoViagem l + tempoEspera l

-- Exercício 2
type Poligonal = [Ponto]
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show, Eq)
-- Definições de funções auxiliares
-- Calcula a distância entre um ponto e o eixo x.

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar raio alfa) = if alfa == pi/2 then 0 else raio * sin alfa

-- Calcula a distância entre um ponto e o eixo y.

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar raio alfa) = if alfa == pi then 0 else raio * cos alfa

-- Calcula a distância de um ponto à origem.

distanciaOrigem :: Ponto -> Double
distanciaOrigem (Cartesiano x y) = sqrt (x^2 + y^2)
distanciaOrigem (Polar raio alfa) = raio

-- Calcula a distância entre dois pontos.

distancia :: Ponto -> Ponto -> Double
distancia (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ((y2 - y1)^2 + (x2 - x1)^2)
distancia (Polar raio1 alfa1) (Polar raio2 alfa2) = 
    sqrt(((raio2 * cos alfa2) - (raio1 * cos alfa1))^2 + ((raio2 * sin alfa2) - (raio1 * sin alfa1))^2)

-- Calcula o valor do ângulo entre o vetor da origem ao ponto com o eixo x.

angulo :: Ponto -> Double
angulo (Cartesiano x y) | x == 0 && y == 0 = 0
                        | x > 0 && y == 0 = 0
                        | x < 0 && y == 0 = pi
                        | x == 0 && y > 0 = pi/2
                        | x == 0 && y < 0 = 3 * pi/2
angulo (Polar raio alfa) = alfa

-- Definição de Figura
data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)
-- Definição de área
area:: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = distancia p1 p2
        b = distancia p2 p3
        c = distancia p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- Formula de Heron
area (Circulo ponto raio) = pi*raio^2
area (Rectangulo p1 p2) = abs(posx p1 - posx p2) * abs(posy p1 - posy p2)
-- Exercício 2.a

comprimentoLinha :: Poligonal -> Double
comprimentoLinha [] = 0
comprimentoLinha [x] = 0
comprimentoLinha (h:t) = distancia h (head t) + comprimentoLinha t

-- Exercício 2.b

linhaFechada :: Poligonal -> Bool
linhaFechada l@(h:t) = length l >= 3 && h == last t

-- Exercício 2.c

triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:t) = if p1 == p3 then [] else Triangulo p1 p2 p3 : triangula (p1:p3:t)
triangula _ = []

-- Exercício 2.d

areaPoligonal :: Poligonal -> Double
areaPoligonal p = areaTris (triangula p)

areaTris :: [Figura] -> Double
areaTris [] = 0
areaTris (h:t) = area h + areaTris t

-- Exercício 2.e

mover :: Poligonal -> Ponto -> Poligonal
mover pol ponto = ponto : pol

-- Exercício 2.f

multiplicado :: Double -> Poligonal -> Poligonal
multiplicado valor (h:t) = mover (multiplicadoAuxiliar valor h t) h

multiplicadoAuxiliar :: Double -> Ponto -> Poligonal -> Poligonal
multiplicadoAuxiliar valor p [] = []
multiplicadoAuxiliar valor p (h:t) = Cartesiano ((posx h - posx p) * valor + posx p) ((posy h - posy p) * valor + posy p) : multiplicadoAuxiliar valor p t

-- Exercício 3
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]
-- Exercício 3.a

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [Email email])]
acrescEmail n e ((nome,contatos):t) | n == nome = (nome, contatos ++ [Email e]):t 
                                    | otherwise = (nome,contatos): acrescEmail n e t

-- Exercício 3.b

verEmails :: Nome -> Agenda -> Maybe [String] -- Maybe ----> Just a | Nothing, em que a = Int/[]/[String]/etc.
verEmails nome [] = Nothing
verEmails n ((nome,contatos):t) | n == nome = Just(getEmails contatos)
                                | otherwise = verEmails n t

getEmails :: [Contacto] -> [String]
getEmails [] = []
getEmails (Email e:t) = e : getEmails t
getEmails (_:t) = getEmails t 

-- Exercício 3.c

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (Tlm n:t) = n : consTelefs t
consTelefs (Trab nm:t) = nm : consTelefs t
consTelefs (Casa nmc:t) = nmc : consTelefs t
consTelefs (_:t) = consTelefs t

-- Exercício 3.d

casa :: Nome -> Agenda -> Maybe Integer
casa nome [] = Nothing
casa n ((nome,contatos):t) | n == nome = getCasa contatos
                           | otherwise = casa n t

getCasa :: [Contacto] -> Maybe Integer
getCasa [] = Nothing
getCasa (Casa nmc:t) = Just nmc 
getCasa (_:t) = getCasa t

-- Exercício 4

type Dia = Int
type Mes = Int
type Ano = Int
type Name = String

data Data = D Dia Mes Ano
            deriving Show
type TabDN = [(Name,Data)]
-- Exercício 4.a

procura :: Name -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome ((n,d):t) | nome == n = Just d
                       | otherwise = procura nome t

-- Exercício 4.b

idade :: Data -> Name -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade (D d m a) nome ((n,D dx mx ax):t) | nome == n = Just (calcularIdade (D d m a) (D dx mx ax))
                                        | otherwise = idade (D d m a) nome t

calcularIdade :: Data -> Data -> Int
calcularIdade (D dd md ad) (D dt mt at) = if mt > md || mt == md && dt > dd then at - ad else at - ad - 1

-- Exercício 4.c

anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2) = a1 < a2 || (a1 == a2 && (m1 < m2 || (m1 == m2 && d1 < d2)))

-- Exercício 4.d

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((nome, date):t) = inserir (nome, date) (ordena t)

inserir :: (Name,Data) -> TabDN -> TabDN
inserir (n,d) [] = [(n,d)]
inserir (n1,d1) ((n2,d2):t) | anterior d1 d2 = (n1,d1) : (n2,d2) : t
                            | otherwise = (n2,d2) : inserir (n1,d1) t

-- Exercício 4.e

porIdade :: Data -> TabDN -> [(Name, Int)]
porIdade date ((n,d):t) = porIdadeAux date (ordena ((n,d):t))

porIdadeAux :: Data -> TabDN -> [(Name,Int)]
porIdadeAux _ [] = []
porIdadeAux d1 ((n,d2):t) = porIdadeAux d1 t ++ [(n, calcularIdade d2 d1)]  

-- Exercício 5
data Movimento = Credito Float | Debito Float
            deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]
            deriving Show
-- Exercício 5.a 

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext startcash ((_,_,mov):t)) valor = if extValorAux mov > valor 
                                                    then mov : extValor (Ext startcash t) valor  
                                                            else extValor (Ext startcash t) valor

extValorAux :: Movimento -> Float
extValorAux (Credito x) = x
extValorAux (Debito x) = x

-- Exercício 5.b

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext startcash ((date,desc,mov):t)) descricao = if desc `elem` descricao 
                                                            then (date,mov) : filtro (Ext startcash t) descricao
                                                                else filtro (Ext startcash t) descricao

-- Exercício 5.c
-- Não percebi o where

creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext startcash ((_,_,Credito x):t)) = (x + cr, db) where (cr,db) = creDeb (Ext startcash t)
creDeb (Ext startcash ((_,_,Debito x):t)) = (cr, x + db) where (cr,db) = creDeb (Ext startcash t)

-- Exercício 5.d

saldo :: Extracto -> Float
saldo (Ext startcash []) = startcash
saldo (Ext startcash ((_,_,Debito x):t)) = saldo (Ext (startcash + x) t)
saldo (Ext startcash ((_,_,Credito x):t)) = saldo (Ext (startcash - x) t)