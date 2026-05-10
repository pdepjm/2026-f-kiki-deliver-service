module Library where
import PdePreludat


data Paquete = UnPaquete{
    destinatario :: String,
    peso :: Number,
    delicadeza :: Number,
    esUrgente :: Bool
} deriving Show

peluche :: Paquete
peluche = UnPaquete{destinatario="Ket", peso=1, delicadeza=3, esUrgente=True}

type Vuelo = [Paquete]

paquetesQuePuedeLLevar :: Number -> Vuelo -> [Paquete]
paquetesQuePuedeLLevar magia vuelo = filter (protegible magia) vuelo

protegible :: Number -> Paquete -> Bool
protegible magia paquete = delicadeza paquete < magia



losMayoresA :: Number -> [Number] -> [Number]
losMayoresA limite numeros = filter (> limite) numeros

-- point free
filtrarLosPares :: [Number] -> [Number]
filtrarLosPares = filter even

restar50 :: Number -> Number
restar50 = flip (-) 50

restar50' :: Number -> Number
restar50' = ( (- 50) +)  

restar50'' :: Number -> Number
restar50'' = (+) (- 50)


elFinal :: String -> String
elFinal = drop 2 


esAfortunado :: Vuelo -> Bool
esAfortunado = all traeSuerte

traeSuerte :: Paquete -> Bool
traeSuerte paquete = even(length(destinatario paquete))

traeSuerte2 :: Paquete -> Bool
traeSuerte2 = even.length.destinatario

--                f            g       f(g(x))    
composicion :: (c -> b) -> (a -> c) -> (a -> b) 
composicion f g x = f (g x)

dificilesDeManiobrar :: [Paquete] ->[Paquete] 
dificilesDeManiobrar = filter esDificilDeManiobrar

esDificilDeManiobrar :: Paquete -> Bool
esDificilDeManiobrar paquete = peso paquete > delicadeza paquete / 2 

dificilesDeManiobrar2 :: [Paquete] ->[Paquete] 
dificilesDeManiobrar2 = filter (\x ->  peso x > delicadeza x / 2 )


type Hechizo = Paquete -> Paquete

reducirPeso :: Number -> Paquete -> Paquete
reducirPeso reduccion paquete = paquete{peso= peso paquete - reduccion}

alivianar :: Hechizo
alivianar = reducirPeso 2

-- Reforzar: Reduce en una cantidad variable el peso (en vez de la delicadeza, como dice el enunciado) del paquete.

reforzar :: Number -> Hechizo
reforzar reduccion = reducirPeso reduccion

hechizoDePaciencia :: Hechizo
hechizoDePaciencia paquete = paquete{esUrgente = False}

type Catalogo = [Hechizo]

catologoDeKiki :: Catalogo
catologoDeKiki = [alivianar, hechizoDePaciencia]

catalogoPotente :: Catalogo
catalogoPotente = [alivianar, reforzar 3, reforzar 10]

simularHechizos :: Paquete -> Catalogo -> [Paquete]
simularHechizos paquete = map ($ paquete)
