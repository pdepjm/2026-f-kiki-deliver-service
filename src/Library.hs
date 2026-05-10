module Library where
import PdePreludat
import Data.Ratio (numerator)

doble :: Number -> Number
doble numero = numero + numero 


data Clima = Lluvia | Nevado | Soleado deriving (Show)
data Paquete = UnPaquete{
    destinatario :: String, 
    peso :: Number,
    delicadeza :: Number, 
    urgente :: Bool 
} deriving Show

ket :: Paquete
ket = UnPaquete {
    destinatario = "Ket",
    peso = 1,
    delicadeza = 3,
    urgente = True 
} 

type Vuelo = [Paquete]

cualesPuedeLLevar :: Vuelo -> Number -> [Paquete]
cualesPuedeLLevar paquetes nivel = filter (esProtegible nivel) paquetes

esProtegible :: Number -> Paquete -> Bool
esProtegible nivelMagia paquete = delicadeza paquete < nivelMagia

recargo :: Clima -> Number -> Paquete -> Number
recargo clima nivelMagia paquete = peso paquete * nivelMagia + factorClimatico clima

factorClimatico :: Clima -> Number 
factorClimatico Lluvia = 30
factorClimatico Nevado = 50
factorClimatico Soleado = 0

esAfortunado :: Vuelo -> Bool
esAfortunado paquetes = all traeSuerte paquetes

traeSuerte :: Paquete -> Bool 
traeSuerte = even.length.destinatario

sonAccesibles :: [Paquete] -> [Paquete]
sonAccesibles paquetes = filter esAccesible paquetes

esAccesible :: Paquete -> Bool
esAccesible = (< 50).recargo Lluvia 10

sonDificilesDeManiobrar :: Vuelo -> [Paquete]
sonDificilesDeManiobrar vuelo = filter dificilDeManiobrar vuelo

dificilDeManiobrar :: Paquete -> Bool
dificilDeManiobrar paquete = peso paquete > delicadeza paquete / 2

sonDificilesDeManiobrar2 :: Vuelo -> [Paquete]
sonDificilesDeManiobrar2 vuelo = filter (\p-> peso p > delicadeza p / 2) vuelo


type Hechizo = Paquete -> Paquete

alivianar :: Hechizo
alivianar paquete = modificarPesoSiPuede (-2) paquete 

modificarPesoSiPuede :: Number -> Paquete -> Paquete 
modificarPesoSiPuede cantidad paquete = paquete{peso = max 0 (pesoModificado paquete cantidad)}

pesoModificado :: Paquete -> Number -> Number 
pesoModificado paquete cantidad = peso paquete + cantidad

reforzar :: Number -> Hechizo 
reforzar cantidad paquete  = paquete{delicadeza = delicadeza paquete - cantidad}


type Catalogo = [Hechizo]

potente :: Catalogo 
potente = [alivianar, reforzar 3]

simularHechizos :: Paquete -> Catalogo -> [Paquete]
simularHechizos paquete catalogo = map ($ paquete) catalogo