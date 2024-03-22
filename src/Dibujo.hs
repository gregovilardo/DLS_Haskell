module Dibujo (
    encimar,
    -- agregar las funciones constructoras
    comp,
    figura,
    apilar,
    juntar,
    rot45,
    rotar,
    espejar,
    (^^^),
    (.-.),
    (///),
    r90,
    r180,
    r270,
    encimar4,
    cuarteto,
    ciclar,
    mapDib,
    change,
    foldDib,
) where

-- nuestro lenguaje
data Dibujo a
    = Figura
    | Rotar (Dibujo a)
    | Espejar (Dibujo a)
    | Rot45 (Dibujo a)
    | Apilar Float Float (Dibujo a) (Dibujo a)
    | Juntar Float Float (Dibujo a) (Dibujo a)
    | Encimar (Dibujo a) (Dibujo a)
    deriving (Eq, Show)

data Figura = Triangulo | Rectangulo deriving (Eq, Show)

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

comp :: Int -> (a -> a) -> a -> a
comp 0 _ x = x
comp n f x = f (comp (n - 1) f x)

-- Funciones constructoras
figura :: a -> Dibujo a
figura _ = Figura

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar f1 f2 d1 d2 = Apilar f1 f2 d1 d2

juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar f1 f2 d1 d2 = Juntar f1 f2 d1 d2

rot45 :: Dibujo a -> Dibujo a
rot45 d = Rot45 d

rotar :: Dibujo a -> Dibujo a
rotar d = Rotar d

espejar :: Dibujo a -> Dibujo a
espejar d = Espejar d

-- Superpone un dibujo con otro.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar

-- Pone el primer dibujo arriba del segundo, ambos ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) d1 d2 = Apilar 1 1 d1 d2

-- Pone un dibujo al lado del otro, ambos ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) d1 d2 = Juntar 1 1 d1 d2

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 = comp 2 rotar

r180 :: Dibujo a -> Dibujo a
r180 = comp 2 r90

r270 :: Dibujo a -> Dibujo a
r270 = comp 6 rotar

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 d = encimar (encimar r1 (r90 r1)) (encimar (r90 (r90 r1)) (r90 (r90 (r90 r1))))
  where
    r1 = espejar (rot45 d)

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto f1 f2 f3 f4 = (.-.) ((///) f1 f2) ((///) f3 f4)

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar = undefined

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib = undefined

-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change = undefined

-- Principio de recursión para Dibujos.
foldDib ::
    (a -> b) ->
    (b -> b) ->
    (b -> b) ->
    (b -> b) ->
    (Float -> Float -> b -> b -> b) ->
    (Float -> Float -> b -> b -> b) ->
    (b -> b -> b) ->
    Dibujo a ->
    b
foldDib = undefined
