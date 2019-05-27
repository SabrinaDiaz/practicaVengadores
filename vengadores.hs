import Text.Show.Functions
import Data.List 

data Personaje = UnPersonaje {nombre::String,
                ataqueFavorito::Personaje->Personaje,
				elementos::[String], 
				energia::Int} deriving (Show)
				
hulk = UnPersonaje "hulk" superFuerza ["pantalones"] 90
thor = UnPersonaje "thor" (relampagos 50) ["mjolnir"] 100
viuda = UnPersonaje "viuda negra" artesMarciales [] 90
capitan = UnPersonaje "capitan america" arrojarEscudo ["escudo"] 80
halcon = UnPersonaje "ojo de halcón" arqueria ["arco", "flechas"] 70
vision = UnPersonaje "vision" (proyectarRayos 5) ["gema del infinito"] 100
ironMan = UnPersonaje "iron man" (ironia (relampagos (-50))) ["armadura", "jarvis", "plata"] 60
ultron = UnPersonaje "robot ultron"  corromperTecnologia [] 100 

--a
esRobot = (comienzaCon "robot".tomarCincoLetras)

tomarCincoLetras personaje = take 5 (nombre personaje)

comienzaCon palabra otraPalabra = palabra == otraPalabra

--b
poseeElemento objeto personaje = elem objeto (elementos personaje) 

--c
potencia personaje = (multiplicarPorLaEnergia personaje.cantidadPosesiones) personaje

multiplicarPorLaEnergia personaje cantidad  = cantidad * energia personaje

cantidadPosesiones personaje = length (elementos personaje)

--2 Ataques

quienGana atacante victima | energia (contraAtaque victima atacante) > energia (ataque atacante victima) = atacante
                           | otherwise = victima

ataque atacante victima = (ataqueFavorito atacante) victima
 
contraAtaque victima atacante = (ataqueFavorito (ataque atacante victima)) atacante

superFuerza personaje = dejarInconsienteAlOponente 0 personaje

dejarInconsienteAlOponente cantidad personaje = personaje { energia = cantidad }

relampagos cantidad personaje = quitarEnergia cantidad personaje

quitarEnergia cantidad personaje = personaje { energia = energia personaje - cantidad }

arqueria personaje | poseeElemento "escudo" personaje = personaje
                   | otherwise = dejarInconsienteAlOponente 0 personaje 
				   
proyectarRayos cantidadExtra personaje = relampagos cantidadExtra personaje

arrojarEscudo personaje = quitarPertenencia personaje

quitarPertenencia personaje = personaje { elementos = [] }

artesMarciales personaje = id personaje

ironia ataque personaje = ataque personaje

corromperTecnologia personaje =( superFuerza.quitarPertenencia) personaje

-- 3 Batalla

enfrentamiento [] _ = [] -- caso base
enfrentamiento _ [] = []  --- caso base
enfrentamiento (avengers1:avengers2) (robots1:robots2) = quienGana avengers1 robots1 : enfrentamiento avengers2 robots2 

--4

avengers = [hulk, thor, viuda, capitan , ultron]

puedeLevantarElMartillo personajes = filter esDigno personajes

esDigno personaje = (nombre personaje) == "stan lee" || not (esRobot personaje) && potencia personaje > 1000 
                  