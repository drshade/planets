module Scripting.ShipScriptInterpreter where
import           Api                  (PlanetUpdate (..), ShipUpdate (..),
                                       TransferTargetType (..),
                                       defaultPlanetUpdate, defaultShipUpdate,
                                       transferTargetType)
import           Control.Monad.Free   (Free (..))
import           Control.Monad.RWS    (MonadState, ask, modify, runRWS, tell)
import           Data.Map             (adjust, alter, empty)
import           Optics               ((^.))
import           Scripting.Model      (Amount (..), Planet, Resource (..), Ship,
                                       gamestatePlanets, getPlanetAtShip,
                                       getPlanetByName, mineralsDuranium,
                                       mineralsMolybdenum, mineralsNeutronium,
                                       mineralsTritanium, planetId, planetName,
                                       planetPosition, planetResources,
                                       positionX, positionY, resourcesClans,
                                       resourcesMegaCredits, resourcesMinerals,
                                       resourcesSupplies, shipId, shipResources)
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
                            (alter (\old ->
                                        let shipUpdate = case old of
                                                Just old' -> old'
                                                Nothing   -> defaultShipUpdate (ship ^. shipId)
                                         in Just $ shipUpdate
                                                    { _shipUpdateX = Just $ planet' ^. planetPosition ^. positionX
                                                    , _shipUpdateY = Just $ planet' ^. planetPosition ^. positionY
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
            let (amtOnShip, amtOnPlanet) = case resource of
                    Clans    -> (ship ^. shipResources ^. resourcesClans, planet ^. planetResources ^. resourcesClans)
                    Mc       -> (ship ^. shipResources ^. resourcesMegaCredits, planet ^. planetResources ^. resourcesMegaCredits)
                    Supplies -> (ship ^. shipResources ^. resourcesSupplies, planet ^. planetResources ^. resourcesSupplies)
                    Neu      -> (ship ^. shipResources ^. resourcesMinerals ^. mineralsNeutronium, planet ^. planetResources ^. resourcesMinerals ^. mineralsNeutronium)
                    Dur      -> (ship ^. shipResources ^. resourcesMinerals ^. mineralsDuranium, planet ^. planetResources ^. resourcesMinerals ^. mineralsDuranium)
                    Tri      -> (ship ^. shipResources ^. resourcesMinerals ^. mineralsTritanium, planet ^. planetResources ^. resourcesMinerals ^. mineralsTritanium)
                    Mol      -> (ship ^. shipResources ^. resourcesMinerals ^. mineralsMolybdenum, planet ^. planetResources ^. resourcesMinerals ^. mineralsMolybdenum)
            -- TBD - Check how much this ship can actually hold and only transfer that
            let amtToTransfer = case amt of
                    Max        -> amtOnPlanet
                    Exact amt' -> min (amt' - amtOnShip) amtOnPlanet
            transfer ship planet resource amtOnShip amtOnPlanet amtToTransfer
            pure ()
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
            transfer ship planet resource amtOnShip amtOnPlanet (-1 * amtToTransfer)
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

transfer :: MonadState ShipScriptState m => Ship -> Planet -> Resource -> Int -> Int -> Int -> m ()
transfer ship planet resource amtOnShip amtOnPlanet amtToTransfer = do
    -- One of the horriblest things i've had to write - its aweful and i promise to do this better
    -- The api is terrible. When tranferring TO PLANETS you must set the *transfer* fields, AND NOT SEND A PLANETUPDATE
    -- but when you are transferring from a planet TO A SHIP you send both updates, but no *transfer* fields... WTF
    modify $ (\(ShipScriptState shipUpdates planetUpdates) ->
                    ShipScriptState
                        (alter
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
                        (alter (
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
                        (adjust (
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
                        (alter (\planetUpdate -> if amtToTransfer < 0 then Nothing else planetUpdate)
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
        alterPlanetUpdate update = case resource of
            Clans       -> update { _planetUpdateClans = Just $ amtOnPlanet - amtToTransfer }
            Mc          -> update { _planetUpdateMegaCredits = Just $ amtOnPlanet - amtToTransfer }
            Supplies    -> update { _planetUpdateSupplies = Just $ amtOnPlanet - amtToTransfer }
            Neu         -> update { _planetUpdateNeutronium = Just $ amtOnPlanet - amtToTransfer }
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

restoreAndRun :: ShipScriptEnvironment -> ShipScript -> (ShipScriptLog, ShipScriptState)
restoreAndRun environment script = do
    let restored = restore environment script
    let shipScriptRWS = interpretRWS restored
    let shipScriptState = ShipScriptState empty empty
    let ((), state, updates) = runRWS shipScriptRWS environment shipScriptState
    (updates, state)

