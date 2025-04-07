
module Main where

import           Api              (ApiKey, GameId, Planet, Planets, Ship,
                                   currentTurn, hullCargo, loadturnRst, login,
                                   planetName, planetNativeType, rstPlayer,
                                   shipClans, shipId, shipName)
import           Calcs            (ResourceHolder, Resources, cargoUsed,
                                   getHull, myPlanets, myShips, nativeType,
                                   production, resources, totalResources)
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
    let testGameId = "643520"
    let cazGameId = "643510"
    let westGameId = "641474"
    (username, password) <- readCredential
    apikey <- login username password
    turn <- currentTurn apikey westGameId
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

                    putStrLn $ " (" ++ printf "%3d" (ship ^. shipId) ++ " - " ++ ship ^. shipName ++ ")"
                    pure ()
                ) myShips'

    putStrLn "\nTotal:"
    let totalShipResources = totalResources myShips'
    putStrLn $ show totalShipResources

    putStrLn "\nProduction per planet:"
    void $ mapM (\planet -> do
                    putStr $ show $ production planet
                    putStrLn $ " (" ++ planet ^. planetName ++ " - " ++ show (nativeType (planet ^. planetNativeType)) ++ ")"
                    pure ()
                ) myPlanets'
    putStrLn "\nTotal:"
    let totalProduction = foldl (\acc e -> acc <> production e) mempty myPlanets'
    putStrLn $ show totalProduction

    pure ()
