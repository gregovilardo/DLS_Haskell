module Pred (
    Pred,
    cambiar,
    anyDib,
    allDib,
    orP,
    andP,
    falla,
) where

import Dibujo

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Figura x))` rota
-- todos los triángulos.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar p f d = foldr (\x acc -> if p x then mapDib f d)  

-- De las siguientes dos funciones esperaria que anyDib sea mas rapida
-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p d = foldr (\x acc -> p x || acc) False arr
  where
    arr = figuras d

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p d = and arr -- foldr (&&) True arr es equivalente a esto
  where
    arr = map p (figuras d)

-- Los dos predicados se cumplen para el elemento recibido.
-- Pred a -> Pred a -> Bool -> a
andP :: Pred a -> Pred a -> Pred a
andP p1 p2 x = p1 x && p2 x

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p1 p2 x = p1 x || p2 x

falla :: Bool
falla = True
