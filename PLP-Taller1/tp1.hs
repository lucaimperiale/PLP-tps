import Data.List
import Test.HUnit
import Midi.Midi

type Tono         = Integer
type Duracion     = Integer
type Instante     = Integer

data Melodia = 
     Silencio Duracion |
     Nota Tono Duracion |
     Secuencia Melodia Melodia | 
     Paralelo [Melodia]
  deriving (Eq,Show)

-- Funciones auxiliares dadas

foldNat :: a->(a->a)->Integer->a
foldNat caso0 casoSuc n | n == 0 = caso0
      | n > 0 = casoSuc (foldNat caso0 casoSuc (n-1))
      | otherwise = error "El arguemnto de foldNat no puede ser negativo."


-- Funciones pedidas

-- Ejercicio 1

superponer :: Melodia->Duracion->Melodia->Melodia

superponer m1 duracionSilencio m2 = Paralelo [m1,Secuencia (Silencio duracionSilencio) m2]  

-- Sugerencia: usar foldNat
canon :: Duracion->Integer->Melodia->Melodia
canon duracion n m1 = foldNat m1 (\r -> superponer m1 duracion r) (n-1)

secuenciar :: [Melodia] -> Melodia--Se asume que la lista no es vacía.
secuenciar = foldr1 (Secuencia) 

-- Ejercicio 2

canonInfinito :: Duracion->Melodia->Melodia

canonInfinito duracion m1 = foldr (\_ r -> superponer m1 duracion r) m1 [0..]

-- Ejercicio 3

foldMelodia :: (Duracion -> b) ->(Tono -> Duracion -> b) -> (b -> b -> b) -> ([b] -> b) -> Melodia -> b  
--En el caso de Secuencia y Paralelo como utilizamos recursivamente foldMelodia en el tipo tenemos que usar (b->b->b) En vez de (Melodia->Melodia->b)
--Despues lo unico que hace la funcion es aplicar el caso correspondiente al resultado recursivo

foldMelodia cS cN cSec cP p = case p of
                                  (Silencio duracion) -> cS duracion
                                  (Nota tono duracion) -> cN tono duracion
                                  (Secuencia m1 m2) -> cSec (rec m1) (rec m2)
                                  (Paralelo ms) -> cP (map rec ms)
                                  where rec = foldMelodia cS cN cSec cP



-- Ejercicio 4
--En este ejercicio es simplemente aplicar la funcion foldMelodia, cambiando las funciones que toma como argumento.
--Se podrian haber escrito de forma mas compacta (fold (id) (id) etc..) , pero usando funciones lambda consideramos que quedaba mas declarativo.

mapMelodia :: (Tono -> Tono)->Melodia->Melodia
mapMelodia fTono = foldMelodia (Silencio) (\tono duracion -> Nota (fTono tono) duracion) (Secuencia) (Paralelo)

transportar :: Integer -> Melodia -> Melodia
transportar n = mapMelodia ((+) n)

duracionTotal :: Melodia->Duracion
duracionTotal = foldMelodia (id) (\tono duracion -> duracion) (+) (safeMaximum)
  where safeMaximum x = if x == [] then 0 else maximum x

cambiarVelocidad :: Float->Melodia->Melodia--Sugerencia: usar round y fromIntegral
cambiarVelocidad factor  = foldMelodia (\duracion -> Silencio (round (fromIntegral duracion * factor))) (\tono duracion -> Nota tono (round (fromIntegral duracion * factor))) (Secuencia) (Paralelo)

invertir :: Melodia -> Melodia
invertir = foldMelodia (Silencio) (Nota) (flip Secuencia) (Paralelo) 

-- Ejercicio 5

-- En instantes menores que 0 no suena ninguna nota. Se puede usar recursión explícita. Resaltar las partes del código que hacen que no se ajuste al esquema fold.
notasQueSuenan :: Instante->Melodia->[Tono]
--Sugerencia: usar concatMap.



notasQueSuenan n p | n < 0 = []
notasQueSuenan n p = case p of
  (Silencio d) -> []
  (Nota t d) -> if n < d then [t] else []
  (Secuencia m1 m2) -> if duracionTotal m1 > n then notasQueSuenan n m1 else notasQueSuenan (n-duracionTotal m1) m2
  (Paralelo ms) -> nub $ concatMap (notasQueSuenan n) ms



--notasQueSuenan 0 _ = []
--notasQueSuenan _ (Silencio d) = []
--notasQueSuenan n (Nota t d) = if n <= d then [t] else []
--notasQueSuenan n (Secuencia m1 m2) = if duracionTotal m1 >= n then notasQueSuenan n m1 else notasQueSuenan (n-duracionTotal m1) m2  
--notasQueSuenan n (Paralelo ms) = nub $ concatMap (notasQueSuenan n) ms
--En este ejercicio vimos de que manera hay que ver si el instante estaba dentro de la melodia dependiendo de la entrada, el unico caso no obvio es Secuencia donde tuvimos que ver si el isntante caia en m1 o m2
--No se podria usar foldMelodia, ya que se tiene que tomar en cuenta el n dentro de la recursion, y no habria forma de pasarlo como parametro de las funciones.
--Intentando que el fold devulva una funcion (Instante -> [Tono]) tendriamos el mismo problema, no podriamos pasar el n en el caso recursivo.


-- Ejercicio 6

data Evento = On Instante Tono | Off Instante Tono deriving (Show, Eq)

--Sugerencia: usar listas por comprensión. No repetir eventos.
cambios :: [Tono]->[Tono]->Instante->[Evento]
cambios tonos1 tonos2 i = [if elem x ts1 then Off i x else On i x | x<- ts1 ++ ts2, (elem x ts1 && not (elem x ts2) ) || (not (elem x ts1) && elem x ts2)]  
  where 
  ts1 = nub tonos1
  ts2 = nub tonos2



--Sugerencia: usar foldl sobre la lista de 0 a la duración.
--Al final de la duracion se tienen que "apagar" las notas,
-- pensamos varias formas de hacer eso pero al final la mas intuitiva que se nos ocurrio fue comprar cambios con una lista vacia  
eventosPorNotas :: (Instante->[Tono])->Duracion->[Evento]
eventosPorNotas fnotas d = (foldl (\rec x -> rec ++ cambios (fnotas $ x-1) (fnotas x) x) [] [0..d]) ++ (cambios (fnotas d) [] (d+1))



eventos :: Melodia -> Duracion -> [Evento]
eventos m1 d = eventosPorNotas (flip notasQueSuenan m1) d 

-- GENERADOR

unev (On i x)  = (i, Left x)
unev (Off i x) = (i, Right x)

generarMidi :: String -> [Evento] -> IO ()
generarMidi archivo eventos = midiCreateFile archivo midiEvents
  where
    eventos' = let e = map unev eventos in zipWith (\(t0, _) (t1, e) -> (t1 - t0, e)) ((0, error ""):e) e
    midiEvents = case eventos' of
                   [] -> [midiNoteOn 1 0 0 10, midiNoteOn 1 0 0 0]
                   es -> toMidi es

toMidi = map (\(d, ev) -> case ev of
                Left  n -> midiNoteOn d 0 n 127
                Right n -> midiNoteOn d 0 n 0)

--Notas para pruebas.

_sol0 = Nota 55
_si0  = Nota 59
_do = Nota 60
_reb  = Nota 61
_re = Nota 62
_mib  = Nota 63
_mi = Nota 64
_fa = Nota 65
_fas  = Nota 66
_sol = Nota 67
_lab  = Nota 68
_la = Nota 69
_sib  = Nota 70
_si = Nota 71
_do2  = Nota 72
_reb2 = Nota 73
_re2  = Nota 74
_mib2 = Nota 75
_fa2  = Nota 77

-- Melodías para pruebas.

acorde, acordeTransportado5, acordeLento, acordeRapido, acordeInvertido :: Melodia
acorde = Paralelo [_do 10, Secuencia (Silencio 3) (_mi 7), Secuencia (Silencio 6) (_sol 4)]
acordeTransportado5 = Paralelo [_fa 10, Secuencia (Silencio 3) (_la 7), Secuencia (Silencio 6) (_do2 4)]
acordeLento = Paralelo [_do 5, Secuencia (Silencio 2) (_mi 4), Secuencia (Silencio 3) (_sol 2)]
acordeRapido = Paralelo [_do 20, Secuencia (Silencio 6) (_mi 14), Secuencia (Silencio 12) (_sol 8)]
acordeInvertido = Paralelo [_do 10, Secuencia (_mi 7) (Silencio 3), Secuencia (_sol 4) (Silencio 6)]

doremi, rebmibfa :: Melodia
doremi = secuenciar [_do 3, _re 1, _mi 3, _do 1, _mi 2, _do 2, _mi 4]
rebmibfa = secuenciar [_reb 3, _mib 1, _fa 3, _reb 1, _fa 2, _reb 2, _fa 4]

do10, re10 :: Melodia
do10 = _do 10
re10 = _re 10

silencio10, silencio20 :: Melodia
silencio10 = Silencio 10
silencio20 = Silencio 20

-- Canon APL (autor: Pablo Barenbaum)

rhoRhoRhoOfX, alwaysEqualsOne, rhoIsDimensionRhoRhoRank, aplIsFun :: Melodia
rhoRhoRhoOfX = secuenciar $ map (\(d, f)->f d) [(4, _do), (4, _do), (3, _do), (1, _re), (4, _mi)] --duracion = 16
alwaysEqualsOne = secuenciar $ map (\(d, f)->f d) [(3, _mi), (1, _re), (3, _mi), (1, _fa), (8, _sol)] --duracion = 16
rhoIsDimensionRhoRhoRank = secuenciar $ map (\(d, f)->f d) [(12, _do2), (12, _sol), (12, _mi), (12, _do)] --duracion = 48
aplIsFun = secuenciar $ map (\(d, f)->f d) [(3, _sol), (1, _fa), (3, _mi), (1, _re), (8, _do)] --duracion = 16

mezcla :: Melodia
mezcla = Paralelo [rhoRhoRhoOfX, Secuencia (Silencio 4) alwaysEqualsOne, Secuencia (Silencio 8) rhoIsDimensionRhoRhoRank, Secuencia (Silencio 12) aplIsFun]

-- Cangrejo (autor: Pablo Barenbaum)

stac :: Tono -> Melodia
stac t = Secuencia (Nota t 9) (Silencio 1)

stacatto :: Melodia -> Melodia
stacatto = foldMelodia Silencio (\t d->stac t) Secuencia Paralelo

cangrejo1 = secuenciar $ 
         [Silencio 4, _do 2, _mib 2]
      ++ [_sol 2, _lab 4, Silencio 2]
      ++ [_si0 4, Silencio 2, _sol 4] 
      ++ [_fas 4, _fa 4]              
      ++ [_mi 2, Silencio 2, _mib 4]  
      ++ [_re 2, _reb 2, _do 2]
      ++ [_si0 2, _sol0 2, _do 2, _fa 2]
      ++ [_mib 2, _re 4, Silencio 2]
      ++ [_do 2, _mi 2, Silencio 4]
cangrejo2 = secuenciar $ (map (\(d, f)->f d)) $
               [(2, _do), (2, _mib), (2, _sol), (2, _do2)]
            ++ [(1, _sib), (1, _do2), (1, _re2), (1, _mib2),
                (1, _fa2), (1, _mib2), (1, _re2), (1, _do2)]
            ++ [(1, _re2), (1, _sol), (1, _re2), (1, _fa2),
                (1, _mib2), (1, _re2), (1, _do2), (1, _si)]
            ++ [(1, _la), (1, _si), (1, _do2), (1, _mib2),
                (1, _re2), (1, _do2), (1, _si), (1, _la)]
            ++ [(1, _sol), (1, _lab), (1, _sib), (1, _reb2),
                (1, _do2), (1, _sib), (1, _lab), (1, _sol)]
            ++ [(1, _fa), (1, _sol), (1, _lab), (1, _sib),
                (1, _lab), (1, _sol), (1, _fa), (1, _mib)]
            ++ [(1, _re), (1, _mib), (1, _fa), (1, _sol),
                (1, _fa), (1, _mib), (1, _re), (1, _lab)]
            ++ [(1, _sol), (1, _fa), (1, _mib), (1, _do2),
                (1, _si), (1, _la), (1, _sol), (1, _fa)]
            ++ [(1, _mi), (1, _re), (1, _mi), (1, _sol),
                (1, _do2), (1, _sol), (1, _fa), (1, _sol)]
cangrejoP = Paralelo $ 
         [Silencio 4, _do 2, _mib 2]
      ++ [_sol 2, _lab 4, Silencio 2]
      ++ [_si0 4, Silencio 2, _sol 4] 
      ++ [_fas 4, _fa 4]              
      ++ [_mi 2, Silencio 2, _mib 4]  
      ++ [_re 2, _reb 2, _do 2]
      ++ [_si0 2, _sol0 2, _do 2, _fa 2]
      ++ [_mib 2, _re 4, Silencio 2]
      ++ [_do 2, _mi 2, Silencio 4]

melodiaTest1 = Paralelo [doremi,acorde]
melodiaTest2 = Paralelo [Secuencia (Silencio 5) (_mi 5), Secuencia (Silencio 1) (_do 2)]
melodiaTest3 = secuenciar $ 
            [Silencio 2, _do 2, Silencio 2]
        ++  [Silencio 2, _do 2, Silencio 2]
        ++  [Silencio 2, _do 2, Silencio 2]
melodiaTest4 = Paralelo [doremi,rebmibfa]    
                
cangrejo = Secuencia c (invertir c)
  where c = Paralelo [cangrejo1, cangrejo2]

--

genMelodia :: String -> Melodia -> Duracion -> IO ()
genMelodia fn m dur = generarMidi fn (eventos m dur)

main :: IO ()
main = do
   putStr "Generando apl-is-fun.mid...\n"
   genMelodia "apl-is-fun.mid" (stacatto mezcla) 500
   putStr "Generando cangrejo.mid...\n"
   genMelodia "cangrejo.mid" (stacatto cangrejo) 1000

-- Tests
tests :: IO Counts
tests = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6
  ]

-- Ejemplos sólo para mostrar cómo se escriben los tests. Reemplazar por los tests propios.

testsEj1 = test [
--superponer
  superponer silencio10 10 silencio10 ~=? Paralelo [silencio10, Secuencia silencio10 silencio10],
  superponer do10 10 re10 ~=? Paralelo [do10, Secuencia silencio10 re10],
  superponer acorde 20 doremi ~=? Paralelo [acorde, Secuencia silencio20 doremi],
  superponer rhoIsDimensionRhoRhoRank 20 aplIsFun ~=? Paralelo [rhoIsDimensionRhoRhoRank, Secuencia silencio20 aplIsFun],
--canon
  canon 10 1 silencio20 ~=? silencio20,
  canon 10 2 silencio20 ~=? Paralelo [silencio20, Secuencia silencio10 silencio20],
  canon 10 3 do10 ~=? Paralelo [do10, Secuencia silencio10 (Paralelo [do10, Secuencia silencio10 do10])],
--secuenciar
  secuenciar [doremi] ~=? doremi,
  secuenciar [doremi, acorde] ~=? Secuencia doremi acorde,
  -- secuenciar [doremi, acorde, mezcla] ~=? Secuencia (Secuencia doremi acorde) mezcla
  secuenciar [doremi, acorde, mezcla] ~=? Secuencia doremi (Secuencia acorde mezcla) 
  ]
testsEj2 = test [
--canonInfinito
  notasQueSuenan 5 (canonInfinito 5 silencio10) ~=? [], 
  notasQueSuenan 1 (canonInfinito 5 acorde) ~=? [60],
  notasQueSuenan 101 (canonInfinito 10 acorde) ~=? [60],
  notasQueSuenan 104 (canonInfinito 10 acorde) ~=? [60,64],
  sort(notasQueSuenan 5 (canonInfinito 1 doremi)) ~=? [60,62,64],
  sort(notasQueSuenan 30 (canonInfinito 1 doremi)) ~=? [60,62,64],
  sort(notasQueSuenan 51 (canonInfinito 1 doremi)) ~=? [60,62,64]
  ]
testsEj3 = test [
  foldMelodia (Silencio) (Nota) (Secuencia) (Paralelo ) silencio10 ~=? silencio10,
  foldMelodia (Silencio) (Nota) (Secuencia) (Paralelo ) do10 ~=? do10,
  foldMelodia (Silencio) (Nota) (Secuencia) (Paralelo ) doremi ~=? doremi,
  foldMelodia (Silencio) (Nota) (Secuencia) (Paralelo ) acorde ~=? acorde
  ]
testsEj4 = test [
 --mapMelodia
  mapMelodia id mezcla ~=? mezcla,
  mapMelodia (2*) silencio10 ~=? silencio10,
  mapMelodia succ doremi ~=? rebmibfa,
--transportar
  transportar 10 silencio10 ~=? silencio10,
  transportar 1 doremi ~=? rebmibfa,
  transportar 5 acorde ~=? acordeTransportado5,
--duracionTotal
  duracionTotal silencio10 ~=? 10,
  duracionTotal acorde ~=? 10,
  duracionTotal doremi ~=? 16,
  duracionTotal mezcla ~=? 56,
  duracionTotal cangrejo1 ~=? 72,
  duracionTotal cangrejoP ~=? 4,
  duracionTotal (Paralelo []) ~=? 0,
--cambiarVelocidad
  cambiarVelocidad 2 silencio10 ~=? silencio20,
  cambiarVelocidad 0.5 silencio20 ~=? silencio10,
  cambiarVelocidad 1 acorde ~=? acorde,
  cambiarVelocidad 0.5 acorde ~=? acordeLento,
  cambiarVelocidad 2 acorde ~=? acordeRapido,
--invertir
  invertir silencio10 ~=? silencio10,
  invertir acorde ~=? acordeInvertido,
  invertir acordeInvertido ~=? acorde
  ]
testsEj5 = test [
--notasQueSuenan
  notasQueSuenan (-1) silencio10 ~=? [],
  notasQueSuenan 5 silencio10 ~=? [],
  notasQueSuenan 0 doremi ~=? [60],
  notasQueSuenan 3 doremi ~=? [62],
  notasQueSuenan 1 acorde ~=? [60],
  notasQueSuenan 4 acorde ~=? [60,64],
  notasQueSuenan 8 acorde ~=? [60,64,67],
  notasQueSuenan 13 doremi ~=? [64]
  ]
testsEj6 = test [
--cambios
  cambios [] [] 50 ~=? [],
  cambios [] [5] 10 ~=? [On 10 5],
  cambios [] [1,2,3] 5 ~=? [On 5 1, On 5 2, On 5 3],
  cambios [5] [] 10 ~=? [Off 10 5],
  cambios [1,2,3] [] 5 ~=? [Off 5 1, Off 5 2, Off 5 3],
  cambios [1,2,3,4,5] [1,2,7,5,7,4,9] 1 ~=? [Off 1 3, On 1 7, On 1 9],
--eventosPorNotas ( Instante -> [Tono])
  eventosPorNotas (flip notasQueSuenan doremi) 0 ~=? [On 0 60,Off 1 60],
  eventosPorNotas (flip notasQueSuenan doremi) 8 ~=? [On 0 60,Off 3 60,On 3 62,Off 4 62,On 4 64,Off 7 64,On 7 60,Off 8 60,On 8 64,Off 9 64],
  eventosPorNotas (flip notasQueSuenan doremi) 16 ~=? [On 0 60,Off 3 60,On 3 62,Off 4 62,On 4 64,Off 7 64,On 7 60,Off 8 60,On 8 64,Off 10 64,On 10 60,Off 12 60,On 12 64,Off 16 64],
  eventosPorNotas (flip notasQueSuenan doremi) 20 ~=? [On 0 60,Off 3 60,On 3 62,Off 4 62,On 4 64,Off 7 64,On 7 60,Off 8 60,On 8 64,Off 10 64,On 10 60,Off 12 60,On 12 64,Off 16 64],
  eventosPorNotas (flip notasQueSuenan acorde) 1 ~=? [On 0 60,Off 2 60],
  eventosPorNotas (flip notasQueSuenan acorde) 4 ~=? [On 0 60,On 3 64,Off 5 60,Off 5 64],
  eventosPorNotas (flip notasQueSuenan acorde) 6 ~=? [On 0 60,On 3 64,On 6 67,Off 7 60,Off 7 64,Off 7 67],
  eventosPorNotas (flip notasQueSuenan acorde) 10 ~=? [On 0 60,On 3 64,On 6 67,Off 10 60,Off 10 64,Off 10 67],
--eventos
  eventos acorde 6 ~=? [On 0 60,On 3 64,On 6 67,Off 7 60,Off 7 64,Off 7 67],
  eventos doremi 5 ~=? [On 0 60,Off 3 60,On 3 62,Off 4 62,On 4 64,Off 6 64],
  eventos acorde 10 ~=? [On 0 60,On 3 64,On 6 67,Off 10 60,Off 10 64,Off 10 67]
  ]

{-melodiaTest1 = Paralelo doremi acorde
melodiaTest2 = Paralelo (Secuencia (Silencio 5) (_mi 5)) (Secuencia (Silencio 1)(_do 2)
melodiaTest3 = secuenciar $ 
            [Silencio 2, _do 2, Silencio 2]
          ++[Silencio 2, _do 2, Silencio 2]
          ++[Silencio 2, _do 2, Silencio 2]  -}
