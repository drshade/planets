module Scripting.Updates where

import qualified Api               as Api (Planet, PlanetUpdate (..), Ship,
                                           ShipUpdate (..), loadturnRst,
                                           planetBuildingStarbase,
                                           planetBuiltDefense,
                                           planetBuiltFactories,
                                           planetBuiltMines, planetClans,
                                           planetColChange,
                                           planetColHappyChange,
                                           planetColonistTaxRate, planetDefense,
                                           planetDevelopmentLevel,
                                           planetDuranium, planetFactories,
                                           planetFriendlyCode, planetId,
                                           planetMegaCredits, planetMines,
                                           planetMolybdenum, planetNativeClans,
                                           planetNativeHappyChange,
                                           planetNativeTaxRate,
                                           planetNeutronium, planetPodCargo,
                                           planetPodHullId, planetPodSpeed,
                                           planetReadyStatus, planetSupplies,
                                           planetSuppliesSold,
                                           planetTargetDefense,
                                           planetTargetFactories,
                                           planetTargetMines, planetTargetX,
                                           planetTargetY, planetTritanium,
                                           rstPlanets, rstShips, shipAmmo,
                                           shipClans, shipDuranium, shipEnemy,
                                           shipFriendlyCode, shipId,
                                           shipMegaCredits, shipMission,
                                           shipMission1Target,
                                           shipMission2Target, shipMolybdenum,
                                           shipName, shipNeutronium,
                                           shipPodCargo, shipPodHullId,
                                           shipReadyStatus, shipSupplies,
                                           shipTargetX, shipTargetY,
                                           shipTransferAmmo, shipTransferClans,
                                           shipTransferDuranium,
                                           shipTransferMegaCredits,
                                           shipTransferMolybdenum,
                                           shipTransferNeutronium,
                                           shipTransferSupplies,
                                           shipTransferTargetId,
                                           shipTransferTargetType,
                                           shipTransferTritanium, shipTritanium,
                                           shipWarp, shipWaypoints)
import           Control.Monad.RWS (ask, get)
import qualified Data.Map          as Map
import           Data.Maybe        (fromMaybe, listToMaybe)
import qualified Model             as Model (Planet, Ship, gamestateLoadTurn,
                                             planetId, shipId)
import           Optics            ((^.))
import           Scripting.Types   (ScriptEnvironment (..), ScriptRWS,
                                    ScriptState (..))

-- If we already have a ship update, then return it
-- otherwise create a new one populated by the turn data
getShipUpdate :: Model.Ship -> ScriptRWS a Api.ShipUpdate
getShipUpdate ship = do
    (ScriptState shipUpdates _) <- get
    case Map.lookup (ship ^. Model.shipId) shipUpdates of
        Just shipUpdate -> pure shipUpdate
        Nothing         -> do
            -- Find the ship in the gamestate and populate the update with initial values
            (ScriptEnvironment gamestate _) <- ask
            let ship' :: Api.Ship
                ship' = fromMaybe (error "Assertion finding ship in gamestate!")
                            $ listToMaybe
                            $ filter (\s -> s ^. Api.shipId == ship ^. Model.shipId) (gamestate ^. Model.gamestateLoadTurn ^. Api.loadturnRst ^. Api.rstShips)
            pure $ Api.ShipUpdate
                { _shipUpdateShipId              = ship' ^. Api.shipId
                , _shipUpdateName                = ship' ^. Api.shipName
                , _shipUpdateNeutronium          = ship' ^. Api.shipNeutronium
                , _shipUpdateDuranium            = ship' ^. Api.shipDuranium
                , _shipUpdateTritanium           = ship' ^. Api.shipTritanium
                , _shipUpdateMolybdenum          = ship' ^. Api.shipMolybdenum
                , _shipUpdateMegaCredits         = ship' ^. Api.shipMegaCredits
                , _shipUpdateSupplies            = ship' ^. Api.shipSupplies
                , _shipUpdateClans               = ship' ^. Api.shipClans
                , _shipUpdateAmmo                = ship' ^. Api.shipAmmo
                , _shipUpdateTransferNeutronium  = ship' ^. Api.shipTransferNeutronium
                , _shipUpdateTransferDuranium    = ship' ^. Api.shipTransferDuranium
                , _shipUpdateTransferTritanium   = ship' ^. Api.shipTransferTritanium
                , _shipUpdateTransferMolybdenum  = ship' ^. Api.shipTransferMolybdenum
                , _shipUpdateTransferMegaCredits = ship' ^. Api.shipTransferMegaCredits
                , _shipUpdateTransferSupplies    = ship' ^. Api.shipTransferSupplies
                , _shipUpdateTransferClans       = ship' ^. Api.shipTransferClans
                , _shipUpdateTransferAmmo        = ship' ^. Api.shipTransferAmmo
                , _shipUpdateTransferTargetId    = ship' ^. Api.shipTransferTargetId
                , _shipUpdateTransferTargetType  = ship' ^. Api.shipTransferTargetType
                , _shipUpdateTargetX             = ship' ^. Api.shipTargetX
                , _shipUpdateTargetY             = ship' ^. Api.shipTargetY
                , _shipUpdateFriendlyCode        = ship' ^. Api.shipFriendlyCode
                , _shipUpdateWarp                = ship' ^. Api.shipWarp
                , _shipUpdateMission             = ship' ^. Api.shipMission
                , _shipUpdateMission1Target      = ship' ^. Api.shipMission1Target
                , _shipUpdateMission2Target      = ship' ^. Api.shipMission2Target
                , _shipUpdatePodHullId           = ship' ^. Api.shipPodHullId
                , _shipUpdatePodCargo            = ship' ^. Api.shipPodCargo
                , _shipUpdateEnemy               = ship' ^. Api.shipEnemy
                , _shipUpdateWaypoints           = ship' ^. Api.shipWaypoints
                , _shipUpdateReadyStatus         = ship' ^. Api.shipReadyStatus
                }

getPlanetUpdate :: Model.Planet -> ScriptRWS a Api.PlanetUpdate
getPlanetUpdate planet = do
    (ScriptState _ planetUpdates) <- get
    case Map.lookup (planet ^. Model.planetId) planetUpdates of
        Just planetUpdate -> pure planetUpdate
        Nothing           -> do
            -- Find the planet in the gamestate and populate the update with initial values
            (ScriptEnvironment gamestate _) <- ask
            let planet' :: Api.Planet
                planet' = fromMaybe (error "Assertion finding planet in gamestate!")
                            $ listToMaybe
                            $ filter (\p -> p ^. Api.planetId == planet ^. Model.planetId) (gamestate ^. Model.gamestateLoadTurn ^. Api.loadturnRst ^. Api.rstPlanets)
            pure $ Api.PlanetUpdate
                { _planetUpdatePlanetId             = planet' ^. Api.planetId
                , _planetUpdateFriendlyCode         = planet' ^. Api.planetFriendlyCode
                , _planetUpdateMines                = planet' ^. Api.planetMines
                , _planetUpdateFactories            = planet' ^. Api.planetFactories
                , _planetUpdateDefense              = planet' ^. Api.planetDefense
                , _planetUpdateTargetMines          = planet' ^. Api.planetTargetMines
                , _planetUpdateTargetFactories      = planet' ^. Api.planetTargetFactories
                , _planetUpdateTargetDefense        = planet' ^. Api.planetTargetDefense
                , _planetUpdateBuiltMines           = planet' ^. Api.planetBuiltMines
                , _planetUpdateBuiltFactories       = planet' ^. Api.planetBuiltFactories
                , _planetUpdateBuiltDefense         = planet' ^. Api.planetBuiltDefense
                , _planetUpdateMegaCredits          = planet' ^. Api.planetMegaCredits
                , _planetUpdateSupplies             = planet' ^. Api.planetSupplies
                , _planetUpdateSuppliesSold         = planet' ^. Api.planetSuppliesSold
                , _planetUpdateNeutronium           = planet' ^. Api.planetNeutronium
                , _planetUpdateDuranium             = planet' ^. Api.planetDuranium
                , _planetUpdateTritanium            = planet' ^. Api.planetTritanium
                , _planetUpdateMolybdenum           = planet' ^. Api.planetMolybdenum
                , _planetUpdateClans                = planet' ^. Api.planetClans
                , _planetUpdateColonistTaxRate      = planet' ^. Api.planetColonistTaxRate
                , _planetUpdateNativeTaxRate        = planet' ^. Api.planetNativeTaxRate
                , _planetUpdateBuildingStarbase     = planet' ^. Api.planetBuildingStarbase
                , _planetUpdateNativeHappyChange    = planet' ^. Api.planetNativeHappyChange
                , _planetUpdateColHappyChange       = planet' ^. Api.planetColHappyChange
                , _planetUpdateColChange            = planet' ^. Api.planetColChange
                , _planetUpdateReadyStatus          = planet' ^. Api.planetReadyStatus
                , _planetUpdatePodHullId            = planet' ^. Api.planetPodHullId
                , _planetUpdatePodCargo             = planet' ^. Api.planetPodCargo
                , _planetUpdatePodSpeed             = planet' ^. Api.planetPodSpeed
                , _planetUpdateNativeClans          = planet' ^. Api.planetNativeClans
                , _planetUpdateTargetX              = planet' ^. Api.planetTargetX
                , _planetUpdateTargetY              = planet' ^. Api.planetTargetY
                , _planetUpdateDevelopmentLevel     = planet' ^. Api.planetDevelopmentLevel
                }
