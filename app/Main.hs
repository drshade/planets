{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import           Api                               (currentTurn, login, update)
import           Calcs                             (production)
import           Control.Monad                     (foldM, void)
import           Data.Data                         (Data)
import           Data.Map                          (empty)
import           Data.Maybe                        (fromMaybe)
import           Model                             (cargoUsed, getPlanetById,
                                                    getShipById, hullCargo,
                                                    myPlanets, myShips,
                                                    planetName,
                                                    planetNativeType,
                                                    planetResources,
                                                    resourcesClans, shipAmmo,
                                                    shipHull, shipId, shipName,
                                                    shipResources,
                                                    totalResources)
import qualified Model                             as Model (fromLoadTurnResponse)
import           MyScripts                         (GameDef (GameDef), scripts)
import           Optics.Operators                  ((^.))
import           Production                        (productionReport)
import           Scripting.PlanetScript            (PlanetScriptEnvironment (..),
                                                    PlanetScriptState (..))
import qualified Scripting.PlanetScriptInterpreter as PlanetScriptInterpreter (restoreAndRun,
                                                                               showPlanetScriptLog)
import           Scripting.ShipScript              (ShipScriptEnvironment (ShipScriptEnvironment),
                                                    ShipScriptState (..))
import qualified Scripting.ShipScriptInterpreter   as ShipScriptInterpreter (restoreAndRun,
                                                                             showShipScriptLog)
import           System.Console.CmdArgs            (cmdArgsMode, cmdArgsRun,
                                                    help, modes, name, program,
                                                    summary, typ, (&=))
import           System.IO.Error                   (tryIOError)
import           Text.Printf                       (printf)
import           System.Directory (getCurrentDirectory)

-- Read from file, first line is username, second line is password
readCredential :: IO (String, String)
readCredential = do
    credentials <- tryIOError $ readFile ".credential"
    case credentials of
        Left _ -> do
            cwd <- getCurrentDirectory
            error $ "Attempting to read .credential file from  working directory: " ++ cwd
        Right contents -> case lines contents of
                username : password : _ -> pure (username, password)
                _ -> error "Expected 2 lines in a file named '.credential'. First line username, second line password."

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
    let gamestate = Model.fromLoadTurnResponse turn

    let myPlanets' = myPlanets gamestate
    let myShips' = myShips gamestate

    putStrLn $ "\nResources per planet:"
    void $ mapM (\planet -> do
                    putStr $ show $ planet ^. planetResources
                    putStrLn $ " (" ++ planet ^. planetName ++ " - " ++ show (planet ^. planetNativeType) ++ ")"
                    pure ()
                ) myPlanets'

    putStrLn "\nTotal:"
    let totalPlanetResources = totalResources myPlanets'
    putStrLn $ show totalPlanetResources

    putStrLn "\nResources per ship:"
    void $ mapM (\ship -> do

                    putStr $ show $ ship ^. shipResources
                    putStr $ " [clans:" ++ printf "%4d" (ship ^. shipResources ^. resourcesClans) ++ "]"

                    let hull = ship ^. shipHull
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
                    putStrLn $ " (" ++ planet ^. planetName ++ " - " ++ show (planet ^. planetNativeType) ++ ")"
                    -- putStrLn $ (showPotentialResources $ potentialProduction planet)
                    pure ()
                ) myPlanets'
    putStrLn "\nTotal:"
    let totalProduction = foldl (\acc e -> acc <> production e) mempty myPlanets'
    putStrLn $ show totalProduction

    pure ()

data Planets
    = RunReport { reportGameid :: String }
    | RunProductionReport { reportGameid :: String }
    | RunScript { }
    deriving (Show, Data)

runReport :: Planets
runReport = RunReport
            { reportGameid = "641474" &= name "gameid" &= help "Id of the game" &= typ "GAMEID" }
            &= help "Display summary of planets & ships"

runProductionReport :: Planets
runProductionReport = RunProductionReport
            { reportGameid = "641474" &= name "gameid" &= help "Id of the game" &= typ "GAMEID" }
            &= help "Display production report of planets"

runScript :: Planets
runScript = RunScript
            { }
            &= help "Display summary of planets & ships"

main :: IO ()
main = do
    mode <- cmdArgsRun $ cmdArgsMode
                    (modes [runReport, runProductionReport, runScript]
                        &= help "Planets tool"
                        &= program "planets"
                        &= summary "Planets Tool v0.1 (by drshade)"
                    )

    (username, password) <- readCredential
    apikey <- login username password

    case mode of
        RunReport gameid -> printSummaryReport gameid
        RunProductionReport gameid -> do
            turn <- currentTurn apikey gameid
            let gamestate = Model.fromLoadTurnResponse turn
            productionReport gamestate
        RunScript -> do
            mapM_
                (\(GameDef gameid shipScripts planetScripts) -> do
                    putStrLn $ "Running scripts for game " <> show gameid

                    loadturn <- currentTurn apikey (show gameid)
                    let gamestate = Model.fromLoadTurnResponse loadturn

                    -- Run the ship scripts
                    (ShipScriptState shipUpdates planetUpdates)
                        <- foldM (\state (shipId', shipScript) -> do
                                let ship = fromMaybe (error $ "Can't find ship! " <> show shipId')
                                                     (getShipById gamestate shipId')
                                let environment = ShipScriptEnvironment ship gamestate
                                let (log', updates) = ShipScriptInterpreter.restoreAndRun environment state shipScript
                                putStrLn $ ShipScriptInterpreter.showShipScriptLog ship log'
                                pure updates
                            ) (ShipScriptState empty empty) shipScripts

                    -- Run the planet scripts, passing in the same state
                    (PlanetScriptState shipUpdates' planetUpdates') <- foldM (\state (planetId', planetScript) -> do
                                let planet = fromMaybe  (error $ "Can't find planet! " <> show planetId')
                                                        (getPlanetById gamestate planetId')
                                let environment = PlanetScriptEnvironment planet gamestate
                                let (log', updates) = PlanetScriptInterpreter.restoreAndRun environment state planetScript
                                putStrLn $ PlanetScriptInterpreter.showPlanetScriptLog planet log'
                                pure updates
                            ) (PlanetScriptState shipUpdates planetUpdates) planetScripts

                    update apikey loadturn shipUpdates' planetUpdates'
                    putStrLn "end"

                    pure ()
                )
                scripts




