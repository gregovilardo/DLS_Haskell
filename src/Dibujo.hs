module Dibujo (
    Dibujo,
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
    -- change,
    foldDib,
    figuras,
) where

-- dibujo -> constructor de tipo (porq toma a)
-- si no tomara nada seria un tipo de dato
-- a -> parametro de tipo
-- nuestro lenguaje
data Dibujo a
    = Figura a
    | Rotar (Dibujo a)
    | Espejar (Dibujo a)
    | Rot45 (Dibujo a)
    | Apilar Float Float (Dibujo a) (Dibujo a)
    | Juntar Float Float (Dibujo a) (Dibujo a)
    | Encimar (Dibujo a) (Dibujo a)
    deriving (Eq, Show)

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

comp :: Int -> (a -> a) -> a -> a
comp 0 _ x = x
comp n f x = f (comp (n - 1) f x)

-- Funciones constructoras
figura :: a -> Dibujo a
figura = Figura

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

rotar :: Dibujo a -> Dibujo a
rotar = Rotar

espejar :: Dibujo a -> Dibujo a
espejar = Espejar

-- Superpone un dibujo con otro.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar

-- Pone el primer dibujo arriba del segundo, ambos ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Apilar 1 1

-- Pone un dibujo al lado del otro, ambos ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Juntar 1 1

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 = comp 2 rotar

r180 :: Dibujo a -> Dibujo a
r180 = comp 4 rotar

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
ciclar f1 = cuarteto f1 (rotar f1) (rotar (rotar f1)) (rotar (rotar (rotar f1)))

-- Dibujo es como una caja
-- map para nuestro lenguaje
mapDib :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib f (Figura a) = f a
mapDib f (Rotar d) = Rotar (mapDib f d)
mapDib f (Espejar d) = Espejar (mapDib f d)
mapDib f (Rot45 d) = Rot45 (mapDib f d)
mapDib f (Apilar x y d1 d2) = Apilar x y (mapDib f d1) (mapDib f d2)
mapDib f (Juntar x y d1 d2) = Juntar x y (mapDib f d1) (mapDib f d2)
mapDib f (Encimar d1 d2) = Encimar (mapDib f d1) (mapDib f d2)

-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función.
-- change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
-- change = undefined

-- En este fold intepreto a `Dibujo a` como la lista [a] en el fold normal
-- no tiene un valor inicial (?)
-- b seria Dibujo a?
-- Principio de recursión para Dibujos.
foldDib ::
    (a -> b) ->
    (b -> b) ->
    (b -> b) ->
    (b -> b) ->
    (Float -> Float -> b -> b -> b) ->
    (Float -> Float -> b -> b -> b) ->
    (b -> b -> b) ->
    Dibujo a -> -- [a] en fold normal ?
    b
foldDib fFig fRot fEsp fRot45 fAp fJun fEnc d =
    case d of
        Figura a -> fFig a
        Rotar a -> fRot (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc a)
        Espejar a -> fEsp (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc a)
        Rot45 a -> fRot45 (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc a)
        (Apilar x y a b) ->
            fAp
                x
                y
                (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc a)
                (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc b)
        (Juntar x y a b) ->
            fJun
                x
                y
                (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc a)
                (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc b)
        (Encimar a b) ->
            fEnc
                (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc a)
                (foldDib fFig fRot fEsp fRot45 fAp fJun fEnc b)

-- Extrae todas las figuras básicas de un dibujo.
figuras :: Dibujo a -> [a]
figuras (Figura d) = [d]
figuras (Rotar d) = figuras d
figuras (Espejar d) = figuras d
figuras (Rot45 d) = figuras d
figuras (Encimar d1 d2) = figuras d1 ++ figuras d2
figuras (Juntar _ _ d1 d2) = figuras d1 ++ figuras d2
figuras (Apilar _ _ d1 d2) = figuras d1 ++ figuras d2
