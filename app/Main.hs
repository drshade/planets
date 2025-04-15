{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import           Api                             (currentTurn, login, update)
import           Calcs                           (production)
import           Control.Monad                   (void)
import           Data.Data                       (Data)
import           Data.Dynamic                    (Typeable)
import           Data.Map                        (empty, unions)
import           Optics.Operators                ((^.))
import qualified Scripting.Model                 as Model (Gamestate,
                                                           fromLoadTurnResponse)
import           Scripting.Model                 (cargoUsed, getPlanetAtShip,
                                                  getShipById, hullCargo,
                                                  myPlanets, myShips,
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
import           Scripts                         (scripts)
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
    | Script { scriptGameid :: String }
    deriving (Show, Data, Typeable)

report :: Planets
report = Report
            { reportGameid = "641474" &= name "gameid" &= help "Id of the game" &= typ "GAMEID" }
            &= help "Display summary of planets & ships"

script :: Planets
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
            loadturn <- currentTurn apikey gameid
            -- let rst = loadturn ^. loadturnRst

            let gamestate :: Model.Gamestate
                gamestate = Model.fromLoadTurnResponse loadturn

            let runScript :: (Int, ShipScript) -> (ShipScriptLog, ShipScriptState)
                runScript = \(shipId', script') ->
                                case getShipById gamestate shipId' of
                                    Nothing -> ([], ShipScriptState empty empty)
                                    Just ship ->
                                        -- Current scripts work when the ship is at a planet
                                        case getPlanetAtShip gamestate ship of
                                            Nothing -> ([], ShipScriptState empty empty)
                                            Just _planet ->
                                                let environment = ShipScriptEnvironment ship gamestate
                                                in restoreAndRun environment script'

            shipScriptStates :: [ShipScriptState]
                <- mapM
                    (\(shipId', script') -> do
                        putStrLn $ "Running script for " <> show shipId'
                        let (log', updates) = runScript (shipId', script')
                        putStrLn $ "Log: " <> show log'
                        pure updates
                    )
                    scripts

            -- Union them together
            let updates = (\(ShipScriptState shipUpdates planetUpdates) -> (shipUpdates, planetUpdates)) <$> shipScriptStates
            let shipUpdates = unions $ (\(shipUpdates', _) -> shipUpdates') <$> updates
            let planetUpdates = unions $ (\(_, planetUpdates') -> planetUpdates') <$> updates

            putStrLn $ "Updates: " <> show updates
            update apikey loadturn shipUpdates planetUpdates
            putStrLn "end"

