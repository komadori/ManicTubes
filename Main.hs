module Main where

import Graphics.QML

main :: IO ()
main = do
    clazz <- newClass []
    object <- newObject clazz ()
    runEngineLoop $ defaultEngineConfig {
        initialDocument = fileDocument "qml/BasicTileTest.qml",
        contextObject = Just $ anyObjRef object
    }
