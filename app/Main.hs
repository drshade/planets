{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import           Api                             (currentTurn, login, update)
import           Calcs                           (production)
import           Control.Monad                   (void)
import           Data.Data                       (Data)
import           Data.Dynamic                    (Typeable)
import           Data.Map                        (empty, unions)
import           Data.Maybe                      (fromMaybe)
import           MyScripts                       (GameDef (GameDef), scripts)
import           Optics.Operators                ((^.))
import qualified Scripting.Model                 as Model (Gamestate,
                                                           fromLoadTurnResponse)
import           Scripting.Model                 (Gamestate, cargoUsed,
                                                  getPlanetAtShip, getShipById,
                                                  hullCargo, myPlanets, myShips,
                                                  planetName, planetNativeType,
                                                  planetResources,
                                                  resourcesClans, shipAmmo,
                                                  shipHull, shipId, shipName,
                                                  shipResources, totalResources)
import           Scripting.ShipScript            (ShipScript,
                                                  ShipScriptEnvironment (ShipScriptEnvironment),
                                                  ShipScriptLog,
                                                  ShipScriptState (..))
import           Scripting.ShipScriptInterpreter (restoreAndRun)
import           System.Console.CmdArgs          (cmdArgsMode, cmdArgsRun, help,
                                                  modes, name, program, summary,
                                                  typ, (&=))
import           System.IO.Error                 (tryIOError)
import           Text.Printf                     (printf)

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
    = Report { reportGameid :: String }
    | Script { }
    deriving (Show, Data, Typeable)

report :: Planets
report = Report
            { reportGameid = "641474" &= name "gameid" &= help "Id of the game" &= typ "GAMEID" }
            &= help "Display summary of planets & ships"

script :: Planets
script = Script
            { }
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
        Script -> do

            (username, password) <- readCredential
            apikey <- login username password

            mapM_
                (\(GameDef gameid shipScripts planetScripts) -> do
                    putStrLn $ "Running script for game " <> show gameid

                    loadturn <- currentTurn apikey (show gameid)
                    let gamestate = Model.fromLoadTurnResponse loadturn

                    shipScriptStates <- mapM (\(shipId, shipScript) -> do
                                let ship = fromMaybe (error $ "Can't find ship! " <> (show shipId))
                                                     (getShipById gamestate shipId)
                                let environment = ShipScriptEnvironment ship gamestate
                                let (log, updates) = restoreAndRun environment shipScript
                                putStrLn $ "Log: " <> show log
                                pure updates

                            ) shipScripts

                    let updates = (\(ShipScriptState shipUpdates planetUpdates) -> (shipUpdates, planetUpdates)) <$> shipScriptStates
                    let shipUpdates = unions $ (\(shipUpdates', _) -> shipUpdates') <$> updates
                    let planetUpdates = unions $ (\(_, planetUpdates') -> planetUpdates') <$> updates

                    update apikey loadturn shipUpdates planetUpdates
                    putStrLn "end"

                    pure ()
                )
                scripts




