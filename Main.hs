{-# LANGUAGE ViewPatterns, DeriveDataTypeable #-}
module Main where

import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Graphics.QML

data Angle = North | East | South | West | None deriving (Eq, Show)

data Orient = Horizontal | Vertical deriving (Eq, Show)

data Tile
    = Start Angle
    | End Angle
    | Corner Angle
    | Straight Orient
    deriving (Eq, Show)

type Point = (Int, Int)

data Grid = Grid Point (Map Point Tile) deriving Show

newGrid :: Grid
newGrid = Grid (0,0) $ Map.fromList [
    ((0,0),Start East),
    ((1,0),Corner East),
    ((1,1),Straight Vertical),
    ((1,2),End South)]

data Plumb = Plumb Point Angle Angle deriving (Typeable, Show)

bend :: Angle -> Angle
bend North = East
bend East = South
bend South = West
bend West = North
bend None = None

invert :: Angle -> Angle
invert = bend . bend

revBend :: Angle -> Angle
revBend = bend . bend . bend

nextPos :: Angle -> Point -> Point
nextPos North (x,y) = (x,y-1)
nextPos East (x,y) = (x+1,y)
nextPos South (x,y) = (x,y+1)
nextPos West (x,y) = (x-1,y)
nextPos None p = p

plumbNext :: Map Point Tile -> Point -> Angle -> Maybe Plumb
plumbNext grid curr flow =
    case Map.lookup curr grid of
        Just (Start theta) -> Just $ Plumb curr None theta
        Just (End theta)
            | flow == theta -> Just $ Plumb curr theta None
        Just (Straight Horizontal)
            | flow == East -> Just $ Plumb curr East West
            | flow == West -> Just $ Plumb curr West East
        Just (Straight Vertical)
            | flow == South -> Just $ Plumb curr North South
            | flow == North -> Just $ Plumb curr South North
        Just (Corner theta)
            | flow == theta      -> Just $ Plumb curr theta (bend theta)
            | flow == bend theta -> Just $ Plumb curr theta (revBend theta)
        _ -> Nothing

plumbGrid :: Grid -> [Plumb]
plumbGrid (Grid start grid) =
    unfoldr f (start, None)
    where f (curr, flow) = do
                plumb@(Plumb _ _ flow') <- plumbNext grid curr flow
                return (plumb, (nextPos flow' curr, flow')) 

angleToInt :: Angle -> Int
angleToInt North = 0
angleToInt East = 90
angleToInt South = 180
angleToInt West = 270
angleToInt None = (-1)

main :: IO ()
main = do
    plumbClazz <- newClass [
        defPropertyConst "x" (\(fromObjRef -> Plumb (x,_) _ _) -> return x),
        defPropertyConst "y" (\(fromObjRef -> Plumb (_,y) _ _) -> return y),
        defPropertyConst "enterAngle" (\(fromObjRef -> Plumb _ theta _) -> return $ angleToInt theta),
        defPropertyConst "exitAngle" (\(fromObjRef -> Plumb _ _ theta) -> return $ angleToInt theta)]
    clazz <- newClass [
        defMethod' "finished" (\_ -> putStrLn "All Done! :-)"),
        defPropertyConst' "plumb" (\_ ->
           mapM (newObject plumbClazz) $ plumbGrid newGrid)]
    object <- newObject clazz ()
    runEngineLoop $ defaultEngineConfig {
        initialDocument = fileDocument "qml/Main.qml",
        contextObject = Just $ anyObjRef object
    }
