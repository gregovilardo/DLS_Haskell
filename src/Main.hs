module Main (main) where

import Control.Monad (when)

-- import Dibujos.Cuadrados (cuadConf)
-- import Dibujos.Ejemplo (ejemploConf)

import Dibujos.Escher (escherConf)
import Dibujos.Feo (feoConf)
import Dibujos.Grilla (grillaConf)
import FloatingPic (Conf (..))
import Interp (initial)
import InterpHaha (ConfH, initialH', simpleHaha)
import InterpSVG (ConfSVG, initialSVG', simpleSVG)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

-- Lista de configuraciones de los dibujos
configs :: [Conf]
configs = [grillaConf, feoConf, escherConf]

configsH :: [ConfH]
configsH = map (\(Conf n p _) -> simpleHaha n p) configs

configsSVG :: [ConfSVG]
configsSVG = map (\(Conf n p _) -> simpleSVG n p) configs

-- Dibuja el dibujo n
initial' :: [Conf] -> String -> IO ()
initial' [] n = do
    putStrLn $ "No hay un dibujo llamado " ++ n
initial' (c : cs) n =
    if n == name c
        then
            initial c 800
        else
            initial' cs n

main :: IO ()
main = do
    args <- getArgs
    when (length args > 2 || null args) $ do
        putStrLn "Sólo puede elegir un dibujo. Para ver los dibujos use -l ."
        exitFailure
    when (head args == "-l") $ do
        putStrLn "Los dibujos disponibles son:"
        mapM_ (putStrLn . name) configs
        exitSuccess
    when (head args == "-a" && not (null $ tail args)) $ do
        initialH' configsH (args !! 1)
        exitSuccess
    when (head args == "-s" && not (null $ tail args)) $ do
        initialSVG' configsSVG (args !! 1)
        exitSuccess
    initial' configs $ head args
