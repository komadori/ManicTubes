module Main where

import Graphics.QML

main :: IO ()
main = do
    clazz <- newClass [
        defMethod' "finished" (\_ -> putStrLn "All Done! :-)")]
    object <- newObject clazz ()
    runEngineLoop $ defaultEngineConfig {
        initialDocument = fileDocument "qml/Main.qml",
        contextObject = Just $ anyObjRef object
    }
