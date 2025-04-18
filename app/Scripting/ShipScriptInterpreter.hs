module Scripting.ShipScriptInterpreter where
import           Api                  (PlanetUpdate (..), ShipUpdate (..),
                                       TransferTargetType (..),
                                       defaultPlanetUpdate, defaultShipUpdate,
                                       planetUpdateClans, planetUpdateDuranium,
                                       planetUpdateMegaCredits,
                                       planetUpdateMolybdenum,
                                       planetUpdateNeutronium,
                                       planetUpdateSupplies,
                                       planetUpdateTritanium, shipUpdateClans,
                                       shipUpdateDuranium,
                                       shipUpdateMegaCredits,
                                       shipUpdateMolybdenum,
                                       shipUpdateNeutronium, shipUpdateSupplies,
                                       shipUpdateTransferClans,
                                       shipUpdateTransferDuranium,
                                       shipUpdateTransferMegaCredits,
                                       shipUpdateTransferMolybdenum,
                                       shipUpdateTransferNeutronium,
                                       shipUpdateTransferSupplies,
                                       shipUpdateTransferTargetId,
                                       shipUpdateTransferTargetType,
                                       shipUpdateTransferTritanium,
                                       shipUpdateTritanium, transferTargetType)
import           Control.Monad.Free   (Free (..))
import           Control.Monad.RWS    (MonadState (get, put), ask, modify,
                                       runRWS, tell)
import           Data.Function        ((&))
import qualified Data.Map             as Map (alter, insert, lookup)
import           Data.Maybe           (fromMaybe)
import           Optics               (_Just, (%), (.~), (^.), (^?))
import           Optics.Lens          (Lens')
import           Scripting.Model      (Amount (..), Planet, Resource (..),
                                       Resources, Ship, engineId,
                                       gamestatePlanets, getPlanetAtShip,
                                       getPlanetByName, hullCargo, hullFuelTank,
                                       mineralsDuranium, mineralsMolybdenum,
                                       mineralsNeutronium, mineralsTritanium,
                                       planetId, planetName, planetOwnerId,
                                       planetPosition, planetResources,
                                       positionX, positionY, resourcesClans,
                                       resourcesMegaCredits, resourcesMinerals,
                                       resourcesSupplies, shipEngine, shipHull,
                                       shipId, shipOwnerId, shipResources)
import           Scripting.ShipScript (ShipScript, ShipScriptEnvironment (..),
                                       ShipScriptInstr (..),
                                       ShipScriptInstruction, ShipScriptLog,
                                       ShipScriptRWS, ShipScriptState (..))

interpretRWS :: ShipScriptInstruction a -> ShipScriptRWS ()
interpretRWS (Pure _) = pure ()
interpretRWS (Free (FlyTo planet _next)) = do
    tell ["FlyTo " <> planet]
    ShipScriptEnvironment ship gamestate <- ask
    case getPlanetByName gamestate planet of
        Just planet' ->
            modify $ (\(ShipScriptState shipUpdates planetUpdates) ->
                        ShipScriptState
                            (Map.alter (\old ->
                                        let shipUpdate = case old of
                                                Just old' -> old'
                                                Nothing   -> defaultShipUpdate (ship ^. shipId)
                                         in Just $ shipUpdate
                                                    { _shipUpdateX = Just $ planet' ^. planetPosition ^. positionX
                                                    , _shipUpdateY = Just $ planet' ^. planetPosition ^. positionY
                                                    -- Set the warp speed to the engines id (1 - 9)
                                                    , _shipUpdateWarp = ship ^? shipEngine % _Just % engineId
                                                    }
                                   )
                                (ship ^. shipId)
                                shipUpdates
                            )
                            planetUpdates
                      )
        Nothing -> pure ()
        -- Do not continue the script
interpretRWS (Free (Pickup amt resource next)) = do
    tell ["Pickup " <> show amt <> " " <> show resource]
    ShipScriptEnvironment ship gamestate <- ask
    case getPlanetAtShip gamestate ship of
        Just planet -> transferTo ship planet resource amt
        Nothing     -> pure ()
    interpretRWS next
interpretRWS (Free (DropOff amt resource next)) = do
    tell ["DropOff " <> show amt <> " " <> show resource]
    ShipScriptEnvironment ship gamestate <- ask
    case getPlanetAtShip gamestate ship of
        Just planet -> transferToPlanet ship planet resource amt
        Nothing     -> pure ()
    interpretRWS next
interpretRWS (Free (GetShip next)) = do
    tell ["GetShip"]
    ShipScriptEnvironment ship _gamestate <- ask
    interpretRWS $ next ship
interpretRWS (Free (GetPlanets next)) = do
    tell ["GetPlanets"]
    ShipScriptEnvironment _ship gamestate <- ask
    let planets = gamestate ^. gamestatePlanets
    interpretRWS $ next planets

shipCurrentCargo :: Ship -> Int
shipCurrentCargo ship =
    ship ^. shipResources ^. resourcesClans
    + ship ^. shipResources ^. resourcesSupplies
    + ship ^. shipResources ^. resourcesMinerals ^. mineralsDuranium
    + ship ^. shipResources ^. resourcesMinerals ^. mineralsTritanium
    + ship ^. shipResources ^. resourcesMinerals ^. mineralsMolybdenum

shipCurrentFuel :: Ship -> Int
shipCurrentFuel ship =
    ship ^. shipResources ^. resourcesMinerals ^. mineralsNeutronium

transferTo :: MonadState ShipScriptState m => Ship -> Planet -> Resource -> Amount -> m ()
transferTo ship planet resource amount = do
    (ShipScriptState shipUpdates planetUpdates) <- get

    -- Do we already have a state update for this ship and/or planet?
    let shipUpdate = fromMaybe (defaultShipUpdate (ship ^. shipId)) $ Map.lookup (ship ^. shipId) shipUpdates
    let planetUpdate = fromMaybe (defaultPlanetUpdate (planet ^. planetId)) $ Map.lookup (planet ^. planetId) planetUpdates

    let (shipUpdate', planetUpdate') = case resource of
            Clans       -> applyUpdate shipUpdate planetUpdate resourcesClans shipUpdateClans planetUpdateClans $ Just (ship ^. shipHull ^. hullCargo - shipCurrentCargo ship) -- limited by cargo
            Mc          -> applyUpdate shipUpdate planetUpdate resourcesMegaCredits shipUpdateMegaCredits planetUpdateMegaCredits Nothing -- no maximum
            Supplies    -> applyUpdate shipUpdate planetUpdate resourcesSupplies shipUpdateSupplies planetUpdateSupplies $ Just (ship ^. shipHull ^. hullCargo - shipCurrentCargo ship) -- limited by cargo
            Neu         -> applyUpdate shipUpdate planetUpdate (resourcesMinerals % mineralsNeutronium) shipUpdateNeutronium planetUpdateNeutronium $ Just $ (ship ^. shipHull ^. hullFuelTank - shipCurrentFuel ship) -- limited by fuel capacity
            Dur         -> applyUpdate shipUpdate planetUpdate (resourcesMinerals % mineralsDuranium) shipUpdateDuranium planetUpdateDuranium $ Just (ship ^. shipHull ^. hullCargo - shipCurrentCargo ship)
            Tri         -> applyUpdate shipUpdate planetUpdate (resourcesMinerals % mineralsTritanium) shipUpdateTritanium planetUpdateTritanium $ Just (ship ^. shipHull ^. hullCargo - shipCurrentCargo ship)
            Mol         -> applyUpdate shipUpdate planetUpdate (resourcesMinerals % mineralsMolybdenum) shipUpdateMolybdenum planetUpdateMolybdenum $ Just (ship ^. shipHull ^. hullCargo - shipCurrentCargo ship)

    put $ ShipScriptState (Map.insert (ship ^. shipId) shipUpdate' shipUpdates) (Map.insert (planet ^. planetId) planetUpdate' planetUpdates)

        where applyUpdate :: ShipUpdate -> PlanetUpdate -> (Lens' Resources Int) -> (Lens' ShipUpdate (Maybe Int)) -> (Lens' PlanetUpdate (Maybe Int)) -> Maybe Int -> (ShipUpdate, PlanetUpdate)
              applyUpdate shipUpdate planetUpdate resourceLens shipUpdateLens planetUpdateLens clamp =
                let shipCurrent = ship ^. shipResources ^. resourceLens
                    planetCurrent = planet ^.planetResources ^. resourceLens
                    amount' = case amount of Exact a -> a; Max -> planetCurrent
                    amt = case clamp of Nothing -> (min (amount') (planetCurrent)); Just maxAmount' -> min (maxAmount') (min (amount') (planetCurrent))
                 in
                    ( shipUpdate & shipUpdateLens .~ Just ((fromMaybe shipCurrent (shipUpdate ^. shipUpdateLens)) + amt)
                    , planetUpdate & planetUpdateLens .~ Just ((fromMaybe planetCurrent (planetUpdate ^. planetUpdateLens)) - amt)
                    )

transferToPlanet :: MonadState ShipScriptState m => Ship -> Planet -> Resource -> Amount -> m ()
transferToPlanet ship planet resource amount = do
    (ShipScriptState shipUpdates planetUpdates) <- get

    -- Do we already have a state update for this ship and/or planet?
    let shipUpdate = fromMaybe (defaultShipUpdate (ship ^. shipId)) $ Map.lookup (ship ^. shipId) shipUpdates

    -- Do we own this planet?
    if (ship ^. shipOwnerId == planet ^. planetOwnerId)
        then do
            -- We can update ship & planet together in a fairly normal way
            let planetUpdate = fromMaybe (defaultPlanetUpdate (planet ^. planetId)) $ Map.lookup (planet ^. planetId) planetUpdates
            let (shipUpdate', planetUpdate') = case resource of
                    Clans       -> applyUpdateToOwned shipUpdate planetUpdate resourcesClans shipUpdateClans planetUpdateClans
                    Mc          -> applyUpdateToOwned shipUpdate planetUpdate resourcesMegaCredits shipUpdateMegaCredits planetUpdateMegaCredits
                    Supplies    -> applyUpdateToOwned shipUpdate planetUpdate resourcesSupplies shipUpdateSupplies planetUpdateSupplies
                    Neu         -> applyUpdateToOwned shipUpdate planetUpdate (resourcesMinerals % mineralsNeutronium) shipUpdateNeutronium planetUpdateNeutronium
                    Dur         -> applyUpdateToOwned shipUpdate planetUpdate (resourcesMinerals % mineralsDuranium) shipUpdateDuranium planetUpdateDuranium
                    Tri         -> applyUpdateToOwned shipUpdate planetUpdate (resourcesMinerals % mineralsTritanium) shipUpdateTritanium planetUpdateTritanium
                    Mol         -> applyUpdateToOwned shipUpdate planetUpdate (resourcesMinerals % mineralsMolybdenum) shipUpdateMolybdenum planetUpdateMolybdenum

            put $ ShipScriptState (Map.insert (ship ^. shipId) shipUpdate' shipUpdates) (Map.insert (planet ^. planetId) planetUpdate' planetUpdates)

        else do
            -- We cannot update the planet, but rather set the "Transfer" fields
            let shipUpdate' = case resource of
                    Clans       -> applyUpdateToNonOwned shipUpdate resourcesClans shipUpdateClans shipUpdateTransferClans
                    Mc          -> applyUpdateToNonOwned shipUpdate resourcesMegaCredits shipUpdateMegaCredits shipUpdateTransferMegaCredits
                    Supplies    -> applyUpdateToNonOwned shipUpdate resourcesSupplies shipUpdateSupplies shipUpdateTransferSupplies
                    Neu         -> applyUpdateToNonOwned shipUpdate (resourcesMinerals % mineralsNeutronium) shipUpdateNeutronium shipUpdateTransferNeutronium
                    Dur         -> applyUpdateToNonOwned shipUpdate (resourcesMinerals % mineralsDuranium) shipUpdateDuranium shipUpdateTransferDuranium
                    Tri         -> applyUpdateToNonOwned shipUpdate (resourcesMinerals % mineralsTritanium) shipUpdateTritanium shipUpdateTransferTritanium
                    Mol         -> applyUpdateToNonOwned shipUpdate (resourcesMinerals % mineralsMolybdenum) shipUpdateMolybdenum shipUpdateTransferMolybdenum

            put $ ShipScriptState (Map.insert (ship ^. shipId) shipUpdate' shipUpdates) planetUpdates

        where applyUpdateToOwned :: ShipUpdate -> PlanetUpdate -> (Lens' Resources Int) -> (Lens' ShipUpdate (Maybe Int)) -> (Lens' PlanetUpdate (Maybe Int)) -> (ShipUpdate, PlanetUpdate)
              applyUpdateToOwned shipUpdate planetUpdate resourceLens shipUpdateLens planetUpdateLens  =
                let shipCurrent = ship ^. shipResources ^. resourceLens
                    planetCurrent = planet ^. planetResources ^. resourceLens
                    amount' = case amount of Exact a -> min a shipCurrent; Max -> shipCurrent
                 in
                    ( shipUpdate & shipUpdateLens .~ Just ((fromMaybe shipCurrent (shipUpdate ^. shipUpdateLens)) - amount')
                    , planetUpdate & planetUpdateLens .~ Just ((fromMaybe planetCurrent (planetUpdate ^. planetUpdateLens)) + amount')

                    )
              applyUpdateToNonOwned :: ShipUpdate -> (Lens' Resources Int) -> (Lens' ShipUpdate (Maybe Int)) -> (Lens' ShipUpdate (Maybe Int)) -> ShipUpdate
              applyUpdateToNonOwned shipUpdate resourceLens shipUpdateLens shipUpdateTransferLens  =
                let shipCurrent = ship ^. shipResources ^. resourceLens
                    amount' = case amount of Exact a -> min a shipCurrent; Max -> shipCurrent
                 in
                    shipUpdate & shipUpdateLens .~ Just ((fromMaybe shipCurrent (shipUpdate ^. shipUpdateLens)) - amount')
                               & shipUpdateTransferLens .~ Just amount'
                               & shipUpdateTransferTargetId .~ Just (planet ^. planetId)
                               & shipUpdateTransferTargetType .~ Just (transferTargetType PlanetTransferTarget)

restore :: ShipScriptEnvironment -> ShipScriptInstruction () -> ShipScriptInstruction ()
restore environment@(ShipScriptEnvironment ship gamestate) instr =
    -- Only restores if the ship is at a particular planet that is referenced in the script (for now!)
    case (\p -> p ^. planetName) <$> getPlanetAtShip gamestate ship of
        Nothing -> pure ()
        Just location ->
            case instr of
                (Pure ())                                       -> pure ()
                (Free (FlyTo planet next)) | planet == location -> next -- found our restore point :)
                                        | otherwise             -> restore environment next
                (Free (Pickup _amt _resource next))             -> restore environment next
                (Free (DropOff _amt _resource next))            -> restore environment next
                (Free (GetShip next))                           -> restore environment (next ship)
                (Free (GetPlanets next))                        -> restore environment (next (gamestate ^. gamestatePlanets))

restoreAndRun :: ShipScriptEnvironment -> ShipScriptState -> ShipScript -> (ShipScriptLog, ShipScriptState)
restoreAndRun environment state script = do
    let restored = restore environment script
    let shipScriptRWS = interpretRWS restored
    -- let shipScriptState = ShipScriptState Map.empty Map.empty
    let ((), state', updates) = runRWS shipScriptRWS environment state
    (updates, state')

