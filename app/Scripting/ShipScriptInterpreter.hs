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
                                       shipUpdateTritanium, transferTargetType)
import           Control.Monad.Free   (Free (..))
import           Control.Monad.RWS    (MonadState (get, put), ask, modify,
                                       runRWS, tell)
import           Data.Function        ((&))
import qualified Data.Map             as Map (adjust, alter, empty, insert,
                                              lookup)
import           Data.Maybe           (fromMaybe)
import           Debug.Trace          (trace)
import           Optics               (_Just, (%), (.~), (^.), (^?))
import           Optics.Lens          (Lens')
import           Scripting.Model      (Amount (..), Planet, Resource (..),
                                       Resources, Ship, engineId,
                                       engineTechLevel, gamestatePlanets,
                                       getPlanetAtShip, getPlanetByName,
                                       hullCargo, hullFuelTank,
                                       mineralsDuranium, mineralsMolybdenum,
                                       mineralsNeutronium, mineralsTritanium,
                                       planetId, planetName, planetPosition,
                                       planetResources, positionX, positionY,
                                       resourcesClans, resourcesMegaCredits,
                                       resourcesMinerals, resourcesSupplies,
                                       shipEngine, shipHull, shipId,
                                       shipResources)
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
        Just planet -> do
            transferTo ship planet resource amt
            -- let (amtOnShip, amtOnPlanet) = case resource of
            --         Clans    -> (ship ^. shipResources ^. resourcesClans, planet ^. planetResources ^. resourcesClans)
            --         Mc       -> (ship ^. shipResources ^. resourcesMegaCredits, planet ^. planetResources ^. resourcesMegaCredits)
            --         Supplies -> (ship ^. shipResources ^. resourcesSupplies, planet ^. planetResources ^. resourcesSupplies)
            --         Neu      -> (ship ^. shipResources ^. resourcesMinerals ^. mineralsNeutronium, planet ^. planetResources ^. resourcesMinerals ^. mineralsNeutronium)
            --         Dur      -> (ship ^. shipResources ^. resourcesMinerals ^. mineralsDuranium, planet ^. planetResources ^. resourcesMinerals ^. mineralsDuranium)
            --         Tri      -> (ship ^. shipResources ^. resourcesMinerals ^. mineralsTritanium, planet ^. planetResources ^. resourcesMinerals ^. mineralsTritanium)
            --         Mol      -> (ship ^. shipResources ^. resourcesMinerals ^. mineralsMolybdenum, planet ^. planetResources ^. resourcesMinerals ^. mineralsMolybdenum)
            -- -- TBD - Check how much this ship can actually hold and only transfer that
            -- let amtToTransfer = case amt of
            --         Max        -> amtOnPlanet
            --         Exact amt' -> min (amt' - amtOnShip) amtOnPlanet
            -- transfer ship planet resource amtOnShip amtOnPlanet amtToTransfer
            -- pure ()
        Nothing     -> pure ()
    interpretRWS next
interpretRWS (Free (DropOff amt resource next)) = do
    tell ["DropOff " <> show amt <> " " <> show resource]
    ShipScriptEnvironment ship gamestate <- ask
    case getPlanetAtShip gamestate ship of
        Just planet -> do
            let (amtOnShip, amtOnPlanet) = case resource of
                    Clans    -> (ship ^. shipResources ^. resourcesClans, planet ^. planetResources ^. resourcesClans)
                    Mc       -> (ship ^. shipResources ^. resourcesMegaCredits, planet ^. planetResources ^. resourcesMegaCredits)
                    Supplies -> (ship ^. shipResources ^. resourcesSupplies, planet ^. planetResources ^. resourcesSupplies)
                    Neu      -> (ship ^. shipResources ^. resourcesMinerals ^. mineralsNeutronium, planet ^. planetResources ^. resourcesMinerals ^. mineralsNeutronium)
                    Dur      -> (ship ^. shipResources ^. resourcesMinerals ^. mineralsDuranium, planet ^. planetResources ^. resourcesMinerals ^. mineralsDuranium)
                    Tri      -> (ship ^. shipResources ^. resourcesMinerals ^. mineralsTritanium, planet ^. planetResources ^. resourcesMinerals ^. mineralsTritanium)
                    Mol      -> (ship ^. shipResources ^. resourcesMinerals ^. mineralsMolybdenum, planet ^. planetResources ^. resourcesMinerals ^. mineralsMolybdenum)
            let amtToTransfer = case amt of
                    Max        -> amtOnShip
                    Exact amt' -> min amt' amtOnShip
            -- transfer ship planet resource amtOnShip amtOnPlanet (-1 * amtToTransfer)
            pure ()
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

    trace ("after => " <> show planetUpdate') $ put $ ShipScriptState (Map.insert (ship ^. shipId) shipUpdate' shipUpdates) (Map.insert (planet ^. planetId) planetUpdate' planetUpdates)

        where applyUpdate :: ShipUpdate -> PlanetUpdate -> (Lens' Resources Int) -> (Lens' ShipUpdate (Maybe Int)) -> (Lens' PlanetUpdate (Maybe Int)) -> Maybe Int -> (ShipUpdate, PlanetUpdate)
              applyUpdate shipUpdate planetUpdate resourceLens shipUpdateLens planetUpdateLens clamp =
                let shipCurrent = ship ^. shipResources ^. resourceLens
                    planetCurrent = planet ^.planetResources ^. resourceLens
                    amount' = case amount of Exact a -> a; Max -> planetCurrent
                    amt = case clamp of Nothing -> (min (amount') (planetCurrent)); Just maxAmount' -> min (maxAmount') (min (amount') (planetCurrent))
                 in trace ("planetUpdate => " <> show planetUpdate)
                    ( shipUpdate & shipUpdateLens .~ Just ((fromMaybe shipCurrent (shipUpdate ^. shipUpdateLens)) + amt)
                    , planetUpdate & planetUpdateLens .~ Just ((fromMaybe planetCurrent (planetUpdate ^. planetUpdateLens)) - amt)
                    )

transfer :: MonadState ShipScriptState m => Ship -> Planet -> Resource -> Int -> Int -> Int -> m ()
transfer ship planet resource amtOnShip amtOnPlanet amtToTransfer = do
    -- One of the horriblest things i've had to write - its aweful and i promise to do this better
    -- The api is terrible. When tranferring TO PLANETS you must set the *transfer* fields, AND NOT SEND A PLANETUPDATE
    -- but when you are transferring from a planet TO A SHIP you send both updates, but no *transfer* fields... WTF
    modify $ (\(ShipScriptState shipUpdates planetUpdates) ->
                    ShipScriptState
                        (Map.alter
                            (\maybeExistingUpdate ->
                                let shipUpdate = case maybeExistingUpdate of
                                        Just existing -> existing
                                        Nothing       -> defaultShipUpdate (ship ^. shipId)
                                in Just $ alterShipUpdate shipUpdate
                            )
                            (ship ^. shipId)
                            shipUpdates
                        )
                        planetUpdates
             )

    modify $ (\(ShipScriptState shipUpdates planetUpdates) ->
                    ShipScriptState
                        shipUpdates
                        (Map.alter (
                            \maybeExistingUpdate ->
                                let planetUpdate = case maybeExistingUpdate of
                                        Just existing -> existing
                                        Nothing -> defaultPlanetUpdate (planet ^. planetId)
                                in Just $ alterPlanetUpdate planetUpdate
                            )
                            (planet ^. planetId)
                            planetUpdates
                        )
             )

    -- If the transfer is from Ship -> Planet - then we set the "transfer target" stuff correctly
    modify $ (\(ShipScriptState shipUpdates planetUpdates) ->
                    ShipScriptState
                        (Map.adjust (
                            \shipUpdate ->
                                if amtToTransfer < 0
                                    then case resource of
                                            Clans       -> shipUpdate { _shipUpdateTransferClans = Just $ (-1 * amtToTransfer), _shipUpdateTransferTargetId = Just $ planet ^. planetId, _shipUpdateTransferTargetType = Just $ transferTargetType PlanetTransferTarget }
                                            Mc          -> shipUpdate { _shipUpdateTransferMegaCredits = Just $ (-1 * amtToTransfer), _shipUpdateTransferTargetId = Just $ planet ^. planetId, _shipUpdateTransferTargetType = Just $ transferTargetType PlanetTransferTarget }
                                            Supplies    -> shipUpdate { _shipUpdateTransferSupplies = Just $ (-1 * amtToTransfer), _shipUpdateTransferTargetId = Just $ planet ^. planetId, _shipUpdateTransferTargetType = Just $ transferTargetType PlanetTransferTarget }
                                            Neu         -> shipUpdate { _shipUpdateTransferNeutronium = Just $ (-1 * amtToTransfer), _shipUpdateTransferTargetId = Just $ planet ^. planetId, _shipUpdateTransferTargetType = Just $ transferTargetType PlanetTransferTarget }
                                            Dur         -> shipUpdate { _shipUpdateTransferDuranium = Just $ (-1 * amtToTransfer), _shipUpdateTransferTargetId = Just $ planet ^. planetId, _shipUpdateTransferTargetType = Just $ transferTargetType PlanetTransferTarget }
                                            Tri         -> shipUpdate { _shipUpdateTransferTritanium = Just $ (-1 * amtToTransfer), _shipUpdateTransferTargetId = Just $ planet ^. planetId, _shipUpdateTransferTargetType = Just $ transferTargetType PlanetTransferTarget }
                                            Mol         -> shipUpdate { _shipUpdateTransferMolybdenum = Just $ (-1 * amtToTransfer), _shipUpdateTransferTargetId = Just $ planet ^. planetId, _shipUpdateTransferTargetType = Just $ transferTargetType PlanetTransferTarget }
                                    else shipUpdate -- unchanged
                            )
                            (ship ^. shipId)
                            shipUpdates
                        )
                        planetUpdates
            )

    -- Delete the planet update if the transfer is towards the planet (I actually think now that this is only required if we
    -- are NOT THE CURRENT OWNER - need to test)
    modify $ (\(ShipScriptState shipUpdates planetUpdates) ->
                    ShipScriptState
                        shipUpdates
                        (Map.alter (\planetUpdate -> if amtToTransfer < 0 then Nothing else planetUpdate)
                            (planet ^. planetId)
                            planetUpdates
                        )
            )

    where
        alterShipUpdate update = case resource of
            Clans       -> update { _shipUpdateClans = Just $ amtOnShip + amtToTransfer }
            Mc          -> update { _shipUpdateMegaCredits = Just $ amtOnShip + amtToTransfer }
            Supplies    -> update { _shipUpdateSupplies = Just $ amtOnShip + amtToTransfer }
            Neu         -> update { _shipUpdateNeutronium = Just $ amtOnShip + amtToTransfer }
            Dur         -> update { _shipUpdateDuranium = Just $ amtOnShip + amtToTransfer }
            Tri         -> update { _shipUpdateTritanium = Just $ amtOnShip + amtToTransfer }
            Mol         -> update { _shipUpdateMolybdenum = Just $ amtOnShip + amtToTransfer }
        alterPlanetUpdate update = trace ("\n" <> show update <> "\n") $ case resource of
            Clans       -> update { _planetUpdateClans = Just $ amtOnPlanet - amtToTransfer }
            Mc          -> update { _planetUpdateMegaCredits = Just $ amtOnPlanet - amtToTransfer }
            Supplies    -> update { _planetUpdateSupplies = Just $ amtOnPlanet - amtToTransfer }
            Neu         -> update { _planetUpdateNeutronium = Just $ maybe (amtOnPlanet - amtToTransfer) (\e -> e - amtToTransfer) (_planetUpdateNeutronium update) }
            Dur         -> update { _planetUpdateDuranium = Just $ amtOnPlanet - amtToTransfer }
            Tri         -> update { _planetUpdateTritanium = Just $ amtOnPlanet - amtToTransfer }
            Mol         -> update { _planetUpdateMolybdenum = Just $ amtOnPlanet - amtToTransfer }

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

