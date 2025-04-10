{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import           Api              (currentTurn, hullCargo, loadturnRst, login,
                                   planetName, planetNativeType, shipAmmo,
                                   shipClans, shipId, shipName)
import           Calcs            (cargoUsed, getHull, myPlanets, myShips,
                                   nativeType, potentialProduction, production,
                                   resources, showPotentialResources,
                                   totalResources)
import           Control.Monad    (void)
import           Optics.Operators ((^.))
import           Text.Printf      (printf)

-- Read from file, first line is username, second line is password
readCredential :: IO (String, String)
readCredential = do
    contents <- readFile ".credential"
    let (username:password:_) = lines contents
    return (username, password)

main :: IO ()
main = do
    let _testGameId = "643520"
    let _cazSmallGameId = "643510"
    let _cazBigGameId = "643598"
    let _westGameId = "641474"

    let gameid = _cazSmallGameId

    (username, password) <- readCredential
    apikey <- login username password
    turn <- currentTurn apikey gameid
    let myPlanets' = myPlanets $ turn ^. loadturnRst
    let myShips' = myShips $ turn ^. loadturnRst

    putStrLn $ "\nResources per planet:"
    void $ mapM (\planet -> do
                    putStr $ show $ resources planet
                    putStrLn $ " (" ++ planet ^. planetName ++ " - " ++ show (nativeType (planet ^. planetNativeType)) ++ ")"
                    pure ()
                ) myPlanets'

    putStrLn "\nTotal:"
    let totalPlanetResources = totalResources myPlanets'
    putStrLn $ show totalPlanetResources

    putStrLn "\nResources per ship:"
    void $ mapM (\ship -> do

                    putStr $ show $ resources ship
                    putStr $ " [clans:" ++ printf "%4d" (ship ^. shipClans) ++ "]"

                    let hull = getHull (turn ^. loadturnRst) ship
                    putStr $ " [cargo:" ++ printf "%4d" (cargoUsed ship) ++ "/" ++ printf "%4d" (hull ^. hullCargo) ++ "]"

                    putStr $ " [ammo:" ++ printf "%2d" (ship ^. shipAmmo) ++ "]"

                    putStrLn $ " (" ++ printf "%3d" (ship ^. shipId) ++ " - " ++ ship ^. shipName ++ ")"
                    pure ()
                ) myShips'

    putStrLn "\nTotal:"
    let totalShipResources = totalResources myShips'
    putStrLn $ show totalShipResources

    putStrLn "\nProduction per planet:"
    void $ mapM (\planet -> do
                    putStr $ (show $ production planet)
                    putStrLn $ " (" ++ planet ^. planetName ++ " - " ++ show (nativeType (planet ^. planetNativeType)) ++ ")"
                    putStrLn $ (showPotentialResources $ potentialProduction planet)
                    pure ()
                ) myPlanets'
    putStrLn "\nTotal:"
    let totalProduction = foldl (\acc e -> acc <> production e) mempty myPlanets'
    putStrLn $ show totalProduction

    pure ()
