{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import           Api                    (Update, currentTurn, hullCargo,
                                         loadturnRst, login, planetName,
                                         planetNativeType, shipAmmo, shipClans,
                                         shipId, shipName, update)
import           Auto                   (GameState (..), Script, interpretGS,
                                         restore, runWithGS)
import           Calcs                  (cargoUsed, getHull, getPlanetAtShip,
                                         getShipById, myPlanets, myShips,
                                         nativeType, potentialProduction,
                                         production, resources,
                                         showPotentialResources, totalResources)
import           Control.Monad          (join, void)
import           Data.Data              (Data)
import           Data.Dynamic           (Typeable)
import           Optics.Operators       ((^.))
import           Scripts                (scripts)
import           System.Console.CmdArgs (cmdArgs, cmdArgsMode, cmdArgsRun, def,
                                         explicit, help, modes, name, opt,
                                         program, summary, typ, (&=))
import           System.Environment     (getArgs)
import           System.IO.Error        (tryIOError)
import           Text.Printf            (printf)

-- Read from file, first line is username, second line is password
readCredential :: IO (String, String)
readCredential = do
    credentials <- tryIOError $ readFile ".credential"
    case credentials of
        Left _ -> error message
        Right contents -> case lines contents of
            username : password : _ -> pure (username, password)
            _                       -> error message
    where message = "Expected 2 lines in a file named '.credential'. First line username, second line password."

printSummaryReport :: String -> IO ()
printSummaryReport gameid = do

    -- My test games
    -- Test game            = 643520
    -- Sector 7777          = 643510
    -- Lets try this thing  = 643598
    -- Westville            = 641474

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

data Planets
    = Report { reportGameid :: String }
    | Script { scriptGameid :: String }
    deriving (Show, Data, Typeable)

report = Report
            { reportGameid = "641474" &= name "gameid" &= help "Id of the game" &= typ "GAMEID" }
            &= help "Display summary of planets & ships"
script = Script
            { scriptGameid = "641474" &= name "gameid" &= help "Id of the game" &= typ "GAMEID" }
            &= help "Display summary of planets & ships"

main :: IO ()
main = do
    mode <- cmdArgsRun $ cmdArgsMode
                    (modes [report, script]
                        &= help "Planets tool"
                        &= program "planets"
                        &= summary "Planets Tool v0.1 (by drshade)"
                    )

    case mode of
        Report gameid -> printSummaryReport gameid
        Script gameid -> do

            (username, password) <- readCredential
            apikey <- login username password
            turn <- currentTurn apikey gameid
            let rst = turn ^. loadturnRst

            let runScript :: (Int, Script) -> [Update]
                runScript = \(shipId, script) ->
                                let Just ship = getShipById rst shipId
                                    Just planet = getPlanetAtShip rst ship
                                    restored = restore (planet ^. planetName) script
                                    updates = runWithGS (interpretGS restored) $ GameState ship turn
                                    in updates

            updates <- join <$> (mapM
                                    (\(shipId, script) -> do
                                        putStrLn $ "Running script for " <> show shipId
                                        let updates = runScript (shipId, script)
                                        pure updates
                                    )
                                    scripts
                                )
            putStrLn $ "Updates: " <> show updates
            update apikey turn updates
            putStrLn "end"

