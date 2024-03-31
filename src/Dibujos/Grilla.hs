module Dibujos.Grilla where

import Dibujo (Dibujo, apilar, encimar, espejar, figura, juntar, rot45, rotar)
import FloatingPic (Conf (..), Output, half, zero)
import Graphics.Gloss (Picture, arc, blue, circle, color, line, pictures, polygon, red, rotate, scale, translate)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- Les ponemos colorcitos para que no sea _tan_ feo
data Color = Azul | Rojo
    deriving (Show, Eq)

data BasicaSinColor = Rectangulo | Cruz | Triangulo | Efe
    deriving (Show, Eq)

data Numeros = Cero | Uno | Dos | Tres | Cuatro | Cinco | Seis | Siete

type Basica = (Numeros, Color)

colorear :: Color -> Picture -> Picture
colorear Azul = color blue
colorear Rojo = color red

-- Las coordenadas que usamos son:
--
--  x + w
--  |
--  x --- x + y
--
-- por ahi deban ajustarlas
interpBasicaSinColor :: Output Numeros
interpBasicaSinColor Uno x w y =
    pictures
        [ line [x V.+ half w V.+ (0.1 V.* y), x V.+ half w V.+ (0.9 V.* y), x V.+ (0.75 V.* y) V.+ (0.25 V.* w)]
        ]
interpBasicaSinColor Cero x w y =
    pictures
        [ line
            [ x V.+ (0.9 V.* y) V.+ half w
            , x V.+ (0.8 V.* y) V.+ (0.25 V.* w)
            , x V.+ (0.5 V.* y) V.+ (0.15 V.* w)
            , x V.+ (0.2 V.* y) V.+ (0.25 V.* w)
            , x V.+ (0.1 V.* y) V.+ half w
            , x V.+ (0.2 V.* y) V.+ (0.75 V.* w)
            , x V.+ (0.5 V.* y) V.+ (0.85 V.* w)
            , x V.+ (0.8 V.* y) V.+ (0.75 V.* w)
            , x V.+ (0.9 V.* y) V.+ half w
            ]
        ]
-- pictures
--     [ line [x V.+ (0.25 V.* y) V.+ (0.25 V.* w), x V.+ (0.25 V.* y) V.+ (0.75 V.* w)]
--     , line [x V.+ (0.75 V.* y) V.+ (0.25 V.* w), x V.+ (0.75 V.* y) V.+ (0.75 V.* w)]
--     , translate xv (yv * 1.5) $ arc 0.0 180.0 200.0
--     , translate xv (yv / 2) $ arc 180.0 0.0 200.0
--     ]

interpBasicaSinColor _ x y w =
    pictures
        [ rotate t (scale 2 1 (circle 50))
        | t <- [0, 30 .. 360]
        ]

-- interpBasicaSinColor Rectangulo x y w = line [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]
-- interpBasicaSinColor Cruz x y w = pictures [line [x, x V.+ y V.+ w], line [x V.+ y, x V.+ w]]
-- interpBasicaSinColor Triangulo x y w = line $ map (x V.+) [(0, 0), y V.+ half w, w, (0, 0)]
-- interpBasicaSinColor Efe x y w =
--     line . map (x V.+) $
--         [ zero
--         , uX
--         , p13
--         , p33
--         , p33 V.+ uY
--         , p13 V.+ uY
--         , uX V.+ 4 V.* uY
--         , uX V.+ 5 V.* uY
--         , x4 V.+ y5
--         , x4 V.+ 6 V.* uY
--         , 6 V.* uY
--         , zero
--         ]
--   where
--     p33 = 3 V.* (uX V.+ uY)
--     p13 = uX V.+ 3 V.* uY
--     x4 = 4 V.* uX
--     y5 = 5 V.* uY
--     uX = (1 / 6) V.* y
--     uY = (1 / 6) V.* w

interpBas :: Output Basica
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

-- Diferentes tests para ver que estén bien las operaciones
figRoja :: Numeros -> Dibujo Basica
figRoja b = figura (b, Rojo)

figAzul :: Numeros -> Dibujo Basica
figAzul b = figura (b, Azul)

-- Debería mostrar un rectángulo azul arriba de otro rojo,
-- conteniendo toda la grilla dentro
apilados :: Numeros -> Dibujo Basica
apilados b = apilar 1 1 (figAzul b) (figRoja b)

-- Debería mostrar un rectángulo azul arriba de otro rojo,
-- conteniendo toda la grilla dentro el primero ocupando 3/4 de la grilla
apilados2 :: Numeros -> Dibujo Basica
apilados2 b = apilar 3 1 (figAzul b) (figRoja b)

-- Debería mostrar un rectángulo azul a derecha de otro rojo,
-- conteniendo toda la grilla dentro
juntados :: Numeros -> Numeros -> Dibujo Basica
juntados a b = juntar 1 1 (figAzul a) (figRoja b)

-- juntados b = juntar 1 1 (figAzul b) (figRoja b)

-- Debería mostrar un rectángulo azul a derecha de otro rojo,
-- conteniendo toda la grilla dentro el primero ocupando 3/4 de la grilla
juntados2 :: Numeros -> Dibujo Basica
juntados2 b = juntar 3 1 (figAzul b) (figRoja b)

-- Igual al anterior, pero invertido
flipante1 :: Numeros -> Dibujo Basica
flipante1 b = espejar $ juntados2 b

-- Igual al anterior, pero invertido
flipante2 :: Numeros -> Dibujo Basica
flipante2 b = espejar $ apilados2 b

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d : ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d : ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

-- cruzTangulo :: Dibujo Basica
-- cruzTangulo = encimar (figRoja Rectangulo) (figAzul Cruz)
--
-- efe :: Dibujo Basica
-- efe = figura (Efe, Azul)

testAll :: Dibujo Basica
testAll =
    grilla [[figAzul Uno]]

-- grilla [[juntados Uno Cero], [juntados Uno Cero], [juntados Uno Cero], [juntados Uno Cero]]

-- [ [cruzTangulo, rot45 cruzTangulo, efe, rot45 efe]
-- , [apilados Rectangulo, apilados2 Rectangulo, juntados Rectangulo, juntados2 Rectangulo]
-- , [flipante1 Rectangulo, flipante2 Rectangulo, figRoja Triangulo, rotar $ figAzul Triangulo]
-- , [rotar $ apilados Efe, apilados2 Efe, juntados Efe, juntados2 Efe]
-- ]

grillaConf :: Conf
grillaConf =
    Conf
        { name = "Grilla"
        , pic = testAll
        , bas = interpBas
        }
