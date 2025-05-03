module Scripting.PlanetScriptInterpreter where
import           Api                    (PlanetUpdate (..),
                                         planetUpdateBuildingStarbase,
                                         planetUpdateBuiltDefense,
                                         planetUpdateBuiltFactories,
                                         planetUpdateBuiltMines,
                                         planetUpdateColonistTaxRate,
                                         planetUpdateDefense,
                                         planetUpdateFactories,
                                         planetUpdateMegaCredits,
                                         planetUpdateMines,
                                         planetUpdateNativeTaxRate,
                                         planetUpdateSupplies)
import           Control.Monad.Free     (Free (..))
import           Control.Monad.RWS      (ask, modify, runRWS, tell)
import qualified Data.Map               as Map
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
import           Scripting.PlanetScript (PlanetScript, PlanetScriptInstr (..),
                                         PlanetScriptInstruction)
import           Scripting.Types        (ScriptEnvironment (ScriptEnvironment),
                                         ScriptLog, ScriptRWS, ScriptState (..))
import           Scripting.Updates      (getPlanetUpdate)


modifyPlanetUpdate :: Planet -> (PlanetUpdate -> PlanetUpdate) -> ScriptRWS Planet ()
modifyPlanetUpdate planet update = do
    planetUpdate <- getPlanetUpdate planet
    modify
        (\(ScriptState shipUpdates planetUpdates) ->
            ScriptState shipUpdates (Map.insert (planet ^. planetId) (update planetUpdate) planetUpdates)
        )

interpret :: PlanetScriptInstruction a -> ScriptRWS Planet ()
interpret (Pure _)                    = pure ()

interpret (Free (GetPlanet next))      = do
    ScriptEnvironment _gamestate planet <- ask
    tell ["GetPlanet => " <> planet ^. planetName <> " (id " <> show (planet ^. planetId) <> ")"]
    interpret $ next planet

interpret (Free (Amount resource next)) = do
    ScriptEnvironment _gamestate planet <- ask
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
    ScriptEnvironment _gamestate planet <- ask
    let amount = planet ^. planetMines
    tell ["Mines => " <> show amount]
    interpret $ next amount

interpret (Free (Factories next)) = do
    ScriptEnvironment _gamestate planet <- ask
    let amount = planet ^. planetFactories
    tell ["Factories => " <> show amount]
    interpret $ next amount

interpret (Free (Defenses next)) = do
    ScriptEnvironment _gamestate planet <- ask
    let amount = planet ^. planetDefenses
    tell ["Defenses => " <> show amount]
    interpret $ next amount

interpret (Free (BuildMines amount next)) = do
    ScriptEnvironment _gamestate planet <- ask

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
                & planetUpdateMines .~ planetUpdate ^. planetUpdateMines + amt
                & planetUpdateBuiltMines .~ planetUpdate ^. planetUpdateBuiltMines + amt
                & planetUpdateMegaCredits .~ planetUpdate ^. planetUpdateMegaCredits - amt * 4
                & planetUpdateSupplies .~ planetUpdate ^. planetUpdateSupplies - amt
        )
    interpret $ next

interpret (Free (BuildFactories amount next)) = do
    ScriptEnvironment _gamestate planet <- ask

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
                & planetUpdateFactories .~ planetUpdate ^. planetUpdateFactories + amt
                & planetUpdateBuiltFactories .~ planetUpdate ^. planetUpdateBuiltFactories + amt
                & planetUpdateMegaCredits .~ planetUpdate ^. planetUpdateMegaCredits - amt * 3
                & planetUpdateSupplies .~ planetUpdate ^. planetUpdateSupplies - amt
        )
    interpret $ next

interpret (Free (BuildDefenses amount next)) = do
    ScriptEnvironment _gamestate planet <- ask

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
                & planetUpdateDefense .~ planetUpdate ^. planetUpdateDefense + amt
                & planetUpdateBuiltDefense .~ planetUpdate ^. planetUpdateBuiltDefense + amt
                & planetUpdateMegaCredits .~ planetUpdate ^. planetUpdateMegaCredits - amt * 10
                & planetUpdateSupplies .~ planetUpdate ^. planetUpdateSupplies - amt
        )
    interpret $ next

interpret (Free (SetNativeTaxRate amount next)) = do
    ScriptEnvironment _gamestate planet <- ask
    modifyPlanetUpdate planet (\planetUpdate ->
            planetUpdate & planetUpdateNativeTaxRate .~ amount
        )
    interpret $ next

interpret (Free (SetColonistTaxRate amount next)) = do
    ScriptEnvironment _gamestate planet <- ask
    modifyPlanetUpdate planet (\planetUpdate ->
            planetUpdate & planetUpdateColonistTaxRate .~ amount
        )
    interpret $ next

interpret (Free (BuildStarbase next)) = do
    ScriptEnvironment _gamestate planet <- ask
    -- TBD: Probably also need to remove the costs of the starbase here... test!
    modifyPlanetUpdate planet (\planetUpdate ->
            planetUpdate & planetUpdateBuildingStarbase .~ True
        )
    interpret $ next

restoreAndRun :: ScriptEnvironment Planet -> ScriptState -> PlanetScript -> (ScriptLog, ScriptState)
restoreAndRun environment state script = do
    -- There is no restore :)
    let restored = script
    let planetScriptRWS = interpret restored
    let ((), state', updates) = runRWS planetScriptRWS environment state
    (updates, state')

showPlanetScriptLog :: Planet -> ScriptLog -> String
showPlanetScriptLog planet logs =
    "Script for planet "
        <> planet ^. planetName
        <> " (id "
        <> show (planet ^. planetId)
        <> "):\n"
        <> (unlines $ (\l -> " -> " <> l) <$> logs)
