{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Api                               (currentTurn, login, update)
import           Calcs                             (cargoUsed, production,
                                                    totalResources)
import           Control.Monad                     (foldM, void)
import           Data.Data                         (Data)
import           Data.Map                          (empty)
import           Data.Maybe                        (fromMaybe)
import           Model                             (Gamestate, getPlanetById,
                                                    getShipById, hullCargo,
                                                    myPlanets, myShips,
                                                    planetName,
                                                    planetNativeType,
                                                    planetResources,
                                                    resourcesClans, shipAmmo,
                                                    shipHull, shipId, shipName,
                                                    shipResources)
import qualified Model                             as Model (fromLoadTurnResponse)
import           MyScripts                         (GameDef (GameDef),
                                                    ScriptAssignment (PlanetScriptAssignment, ShipScriptAssignment),
                                                    scripts)
import           Optics.Operators                  ((^.))
import           Production                        (productionReport)
import qualified Scripting.PlanetScriptInterpreter as PlanetScriptInterpreter (restoreAndRun,
                                                                               showPlanetScriptLog)

import           Config                            (readCredential)
import           Mcp                               (McpPrompt, McpResource,
                                                    McpTool, handlePrompt,
                                                    handleResource, handleTool)
import           MCP.Server                        (runMcpServerStdio)
import           MCP.Server.Derive
import           MCP.Server.Types                  (McpServerHandlers (..),
                                                    McpServerInfo (..))
import qualified Scripting.ShipScriptInterpreter   as ShipScriptInterpreter (restoreAndRun,
                                                                             showShipScriptLog)
import           Scripting.Types                   (ScriptEnvironment (..),
                                                    ScriptState (ScriptState))
import           System.Console.CmdArgs            (cmdArgsMode, cmdArgsRun,
                                                    help, modes, name, program,
                                                    summary, typ, (&=))
import           Text.Printf                       (printf)

-- My test games, oh also my test games, except for those that aren't mine
-- Test game            = 643520
-- Sector 7777          = 643510
-- Lets try this thing  = 643598
-- Westville            = 641474

printSummaryReport :: Gamestate -> IO ()
printSummaryReport gamestate = do

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
    = RunReport { runReportGameid :: String }
    | RunProductionReport { runProductionReportGameid :: String }
    | RunScript { runScriptGameid :: Int }
    | RunMcp
    deriving (Show, Data)

runReport :: Planets
runReport = RunReport
            { runReportGameid = "641474" &= name "gameid" &= help "Id of the game" &= typ "GAMEID" }
            &= help "Display summary of planets & ships"

runProductionReport :: Planets
runProductionReport = RunProductionReport
            { runProductionReportGameid = "641474" &= name "gameid" &= help "Id of the game" &= typ "GAMEID" }
            &= help "Display production report of planets"

runScript :: Planets
runScript = RunScript
            { runScriptGameid = 641474 &= name "gameid" &= help "Id of the game" &= typ "GAMEID" }
            &= help "Display summary of planets & ships"

runMcp :: Planets
runMcp = RunMcp
            { }
            &= help "Run the MCP server"

main :: IO ()
main = do
    mode <- cmdArgsRun $ cmdArgsMode
                    (modes [runReport, runProductionReport, runScript, runMcp]
                        &= help "Planets tool"
                        &= program "planets"
                        &= summary "Planets Tool v0.1 (by drshade)"
                    )

    case mode of
        RunReport gameid -> do
            (username, password) <- readCredential
            apikey <- login username password
            turn <- currentTurn apikey gameid
            let gamestate = Model.fromLoadTurnResponse turn
            printSummaryReport gamestate
        RunProductionReport gameid -> do
            (username, password) <- readCredential
            apikey <- login username password
            turn <- currentTurn apikey gameid
            let gamestate = Model.fromLoadTurnResponse turn
            productionReport gamestate
        RunScript gameid -> do
            (username, password) <- readCredential
            apikey <- login username password
            mapM_
                (\(GameDef thisgameid scriptAssignments) -> do
                    putStrLn $ "Running scripts for game " <> show thisgameid

                    loadturn <- currentTurn apikey (show thisgameid)
                    let gamestate = Model.fromLoadTurnResponse loadturn

                    -- Run the scripts
                    (ScriptState shipUpdates planetUpdates)
                        <- foldM (\state scriptAssignment -> do
                            case scriptAssignment of
                                ShipScriptAssignment shipId' shipScript -> do
                                    -- Get the ship from the gamestate
                                    let ship = fromMaybe (error $ "Can't find ship! " <> show shipId')
                                                         (getShipById gamestate shipId')
                                    let environment = ScriptEnvironment gamestate ship
                                    let (log', updates) = ShipScriptInterpreter.restoreAndRun environment state shipScript
                                    putStrLn $ ShipScriptInterpreter.showShipScriptLog ship log'
                                    pure updates
                                PlanetScriptAssignment planetId' planetScript -> do
                                    -- Get the planet from the gamestate
                                    let planet = fromMaybe (error $ "Can't find planet! " <> show planetId')
                                                           (getPlanetById gamestate planetId')
                                    let environment = ScriptEnvironment gamestate planet
                                    let (log', updates) = PlanetScriptInterpreter.restoreAndRun environment state planetScript
                                    putStrLn $ PlanetScriptInterpreter.showPlanetScriptLog planet log'
                                    pure updates
                            ) (ScriptState empty empty) scriptAssignments

                    update apikey loadturn shipUpdates planetUpdates
                    putStrLn "end"

                    pure ()
                )
                -- Only for games which match the gameid
                $ filter (\(GameDef gameid' _) -> gameid == gameid') scripts
        RunMcp -> do
            let tools = $(deriveToolHandler ''McpTool 'handleTool)
            let resources = $(deriveResourceHandler ''McpResource 'handleResource)
            let prompts = $(derivePromptHandler ''McpPrompt 'handlePrompt)
             in runMcpServerStdio
                    McpServerInfo
                        { serverName = "Planets.nu MCP Server"
                        , serverVersion = "0.1.0"
                        , serverInstructions = "Tools to fetch information about a planets.nu game (VGAPlanets)"
                        }
                    McpServerHandlers
                        { prompts = Just prompts
                        , resources = Just resources
                        , tools = Just tools
                        }

