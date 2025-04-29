module Scripting.PlanetScriptInterpreter where
import           Api                    (PlanetUpdate (..), defaultPlanetUpdate,
                                         planetUpdateBuiltDefense,
                                         planetUpdateBuiltFactories,
                                         planetUpdateBuiltMines,
                                         planetUpdateDefense,
                                         planetUpdateFactories,
                                         planetUpdateMegaCredits,
                                         planetUpdateMines,
                                         planetUpdateSupplies)
import           Control.Monad.Free     (Free (..))
import           Control.Monad.RWS      (MonadState, ask, modify, runRWS, tell)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import           Model                  (Amount (..), Planet, Resource (..),
                                         mineralsDuranium, mineralsMolybdenum,
                                         mineralsNeutronium, mineralsTritanium,
                                         planetDefenses, planetFactories,
                                         planetId, planetMines, planetName,
                                         planetResources, resourcesClans,
                                         resourcesMegaCredits,
                                         resourcesMinerals, resourcesSupplies)
import           Optics.Operators
import           Optics.Optic
import           Production             (maximumDefensesForColonists,
                                         maximumFactoriesForColonists,
                                         maximumMinesForColonists)
import           Scripting.PlanetScript (PlanetScript,
                                         PlanetScriptEnvironment (..),
                                         PlanetScriptInstr (..),
                                         PlanetScriptInstruction,
                                         PlanetScriptLog, PlanetScriptRWS,
                                         PlanetScriptState (..))


modifyPlanetUpdate :: MonadState PlanetScriptState m => Planet -> (PlanetUpdate -> PlanetUpdate) -> m ()
modifyPlanetUpdate planet update =
    modify
        (\(PlanetScriptState shipUpdates planetUpdates) ->
            PlanetScriptState
                shipUpdates
                (Map.alter (\old ->
                            let planetUpdate = case old of
                                    Just old' -> old'
                                    Nothing   -> defaultPlanetUpdate (planet ^. planetId)
                                in Just $ update planetUpdate
                        )
                    (planet ^. planetId)
                    planetUpdates
                )
        )

interpret :: PlanetScriptInstruction a -> PlanetScriptRWS ()
interpret (Pure _)                    = pure ()
interpret (Free (GetPlanet next))      = do
    PlanetScriptEnvironment planet _gamestate <- ask
    tell ["GetPlanet => " <> planet ^. planetName <> " (id " <> show (planet ^. planetId) <> ")"]
    interpret $ next planet
interpret (Free (Amount resource next)) = do
    PlanetScriptEnvironment planet _gamestate <- ask
    let amount = case resource of
            Clans    -> planet ^. planetResources ^. resourcesClans
            Supplies -> planet ^. planetResources ^. resourcesSupplies
            Mc       -> planet ^. planetResources ^. resourcesMegaCredits
            Neu      -> planet ^. planetResources ^. resourcesMinerals ^. mineralsNeutronium
            Dur      -> planet ^. planetResources ^. resourcesMinerals ^. mineralsDuranium
            Tri      -> planet ^. planetResources ^. resourcesMinerals ^. mineralsTritanium
            Mol      -> planet ^. planetResources ^. resourcesMinerals ^. mineralsMolybdenum
    tell ["Amount " <> show resource <> " => " <> show amount]
    interpret $ next amount
interpret (Free (Mines next)) = do
    PlanetScriptEnvironment planet _gamestate <- ask
    let amount = planet ^. planetMines
    tell ["Mines => " <> show amount]
    interpret $ next amount
interpret (Free (Factories next)) = do
    PlanetScriptEnvironment planet _gamestate <- ask
    let amount = planet ^. planetFactories
    tell ["Factories => " <> show amount]
    interpret $ next amount
interpret (Free (Defenses next)) = do
    PlanetScriptEnvironment planet _gamestate <- ask
    let amount = planet ^. planetDefenses
    tell ["Defenses => " <> show amount]
    interpret $ next amount
interpret (Free (BuildMines amount next)) = do
    PlanetScriptEnvironment planet _gamestate <- ask

    -- The maximum amount of mines we can build is limited by
    -- 1. The amount of mines our colonists can support (minus the amount we already have)
    -- 2. The amount of mines we can afford (3x MC + 1x Supply)

    -- BUG: We need to take into account existing updates which may have modified MegaCredits or Supply ... Not from the turn
    let maxMines = min (max 0 $ maximumMinesForColonists (planet ^. planetResources ^. resourcesClans) - (planet ^. planetMines))
                 $ min (planet ^. planetResources ^. resourcesMegaCredits `div` 4)
                       (planet ^. planetResources ^. resourcesSupplies)

    let amt = case amount of Exact a -> min maxMines a; Max -> maxMines

    tell ["BuildMines " <> show amount <> " => " <> show amt <> " (max " <> show maxMines <> ")"]

    modifyPlanetUpdate planet (\planetUpdate ->
            planetUpdate
                & planetUpdateMines .~ Just (fromMaybe (planet ^. planetMines) (planetUpdate ^. planetUpdateMines) + amt)
                & planetUpdateBuiltMines .~ Just (fromMaybe 0 (planetUpdate ^. planetUpdateBuiltMines) + amt)
                & planetUpdateMegaCredits .~ Just (fromMaybe (planet ^. planetResources ^. resourcesMegaCredits) (planetUpdate ^. planetUpdateMegaCredits) - amt * 4)
                & planetUpdateSupplies .~ Just (fromMaybe (planet ^. planetResources ^. resourcesSupplies) (planetUpdate ^. planetUpdateSupplies) - amt)
        )
    interpret $ next

interpret (Free (BuildFactories amount next)) = do
    PlanetScriptEnvironment planet _gamestate <- ask

    -- The maximum amount of factories we can build is limited by
    -- 1. The amount of factories our colonists can support (minus the amount we already have)
    -- 2. The amount of factories we can afford (4x MC + 1x Supply)

    -- BUG: We need to take into account existing updates which may have modified MegaCredits or Supply ... Not from the turn
    let maxFactories = min (max 0 $ maximumFactoriesForColonists (planet ^. planetResources ^. resourcesClans) - (planet ^. planetFactories))
                 $ min (planet ^. planetResources ^. resourcesMegaCredits `div` 3)
                       (planet ^. planetResources ^. resourcesSupplies)

    let amt = case amount of Exact a -> min maxFactories a; Max -> maxFactories

    tell ["BuildFactories " <> show amount <> " => " <> show amt <> " (max " <> show maxFactories <> ")"]

    modifyPlanetUpdate planet (\planetUpdate ->
            planetUpdate
                & planetUpdateFactories .~ Just (fromMaybe (planet ^. planetFactories) (planetUpdate ^. planetUpdateFactories) + amt)
                & planetUpdateBuiltFactories .~ Just (fromMaybe 0 (planetUpdate ^. planetUpdateBuiltFactories) + amt)
                & planetUpdateMegaCredits .~ Just (fromMaybe (planet ^. planetResources ^. resourcesMegaCredits) (planetUpdate ^. planetUpdateMegaCredits) - amt * 3)
                & planetUpdateSupplies .~ Just (fromMaybe (planet ^. planetResources ^. resourcesSupplies) (planetUpdate ^. planetUpdateSupplies) - amt)
        )
    interpret $ next

interpret (Free (BuildDefenses amount next)) = do
    PlanetScriptEnvironment planet _gamestate <- ask

    -- The maximum amount of defenses we can build is limited by
    -- 1. The amount of defenses our colonists can support (minus the amount we already have)
    -- 2. The amount of defenses we can afford (10x MC + 1x Supply)

    -- BUG: We need to take into account existing updates which may have modified MegaCredits or Supply ... Not from the turn
    let maxDefenses = min (max 0 $ maximumDefensesForColonists (planet ^. planetResources ^. resourcesClans) - (planet ^. planetDefenses))
                 $ min (planet ^. planetResources ^. resourcesMegaCredits `div` 10)
                       (planet ^. planetResources ^. resourcesSupplies)

    let amt = case amount of Exact a -> min maxDefenses a; Max -> maxDefenses

    tell ["BuildDefenses " <> show amount <> " => " <> show amt <> " (max " <> show maxDefenses <> ")"]

    modifyPlanetUpdate planet (\planetUpdate ->
            planetUpdate
                & planetUpdateDefense .~ Just (fromMaybe (planet ^. planetDefenses) (planetUpdate ^. planetUpdateDefense) + amt)
                & planetUpdateBuiltDefense .~ Just (fromMaybe 0 (planetUpdate ^. planetUpdateBuiltDefense) + amt)
                & planetUpdateMegaCredits .~ Just (fromMaybe (planet ^. planetResources ^. resourcesMegaCredits) (planetUpdate ^. planetUpdateMegaCredits) - amt * 10)
                & planetUpdateSupplies .~ Just (fromMaybe (planet ^. planetResources ^. resourcesSupplies) (planetUpdate ^. planetUpdateSupplies) - amt)
        )
    interpret $ next

restoreAndRun :: PlanetScriptEnvironment -> PlanetScriptState -> PlanetScript -> (PlanetScriptLog, PlanetScriptState)
restoreAndRun environment state script = do
    -- There is no restore :)
    let restored = script
    let planetScriptRWS = interpret restored
    let ((), state', updates) = runRWS planetScriptRWS environment state
    (updates, state')

showPlanetScriptLog :: Planet -> PlanetScriptLog -> String
showPlanetScriptLog planet logs =
    "Script for planet "
        <> planet ^. planetName
        <> " (id "
        <> show (planet ^. planetId)
        <> "):\n"
        <> (unlines $ (\l -> " -> " <> l) <$> logs)
