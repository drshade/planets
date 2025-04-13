module Auto where
import           Api                    (ApiKey, Hull (..), LoadTurnResponse,
                                         Planet (Planet), Rst, Ship (..),
                                         TransferTargetType (..), Update (..),
                                         defaultPlanetUpdate, defaultShipUpdate,
                                         loadturnRst, planetClans,
                                         planetDuranium, planetId,
                                         planetMegaCredits, planetMolybdenum,
                                         planetNeutronium, planetSupplies,
                                         planetTritanium, planetX, planetY,
                                         rstPlanets, shipClans, shipDuranium,
                                         shipId, shipMegaCredits,
                                         shipMolybdenum, shipName,
                                         shipNeutronium, shipSupplies,
                                         shipTritanium, transferTargetType,
                                         update)
import           Calcs                  (getHull, getPlanetAtShip,
                                         getPlanetByName)
import           Control.Monad.Free     (Free (..), liftF)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.RWS      (RWS, modify, runRWS, tell)
import           Control.Monad.RWS.CPS  (MonadState)
import           Data.Map               (Map, adjust, alter, empty, insert)
import           Model                  (Amount (..), EntityKey (..),
                                         Resource (..))
import           Optics.Operators

type Script = Instruction ()

data GameState = GameState Ship LoadTurnResponse

type ShipId = Int

type AutoM = RWS GameState [String] (Map EntityKey Update)

data Instr next
    = FlyTo String next
    | Pickup Amount Resource next
    | DropOff Amount Resource next
    | GetShip (Ship -> next)
    | GetShipHull Ship (Hull -> next)
    | GetPlanets ([Planet] -> next)
    deriving (Functor)

type Instruction = Free Instr

flyTo :: String -> Instruction ()
flyTo planet = liftF $ FlyTo planet ()

pickup :: Amount -> Resource -> Instruction ()
pickup amt resource = liftF $ Pickup amt resource ()

dropOff :: Amount -> Resource -> Instruction ()
dropOff amt resource = liftF $ DropOff amt resource ()

getShip :: Instruction Ship
getShip = liftF $ GetShip id

getShipHull :: Ship -> Instruction Hull
getShipHull ship = liftF $ GetShipHull ship id

getPlanets :: Instruction [Planet]
getPlanets = liftF $ GetPlanets id

interpret :: Instruction a -> [String]
interpret (Pure _)                   = []
interpret (Free (FlyTo planet next)) = ("FlyTo " <> planet) : interpret next
interpret (Free (Pickup amt resource next))   = ("Pickup " <> show amt <> " " <> show resource) : interpret next
interpret (Free (DropOff amt resource next))  = ("DropOff " <> show amt <> " " <> show resource) : interpret next
interpret (Free (GetShip next)) = ("GetShip") : interpret (next (Ship "" 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
interpret (Free (GetShipHull ship next)) = ("GetShipHull " <> ship ^. shipName) : interpret (next $ Hull "" 0 0)
interpret (Free (GetPlanets next)) = ("GetPlanets") : interpret (next [])

interpretGS :: Instruction a -> AutoM ()
interpretGS (Pure _) = pure ()
interpretGS (Free (FlyTo planet next)) = do
    tell ["FlyTo " <> planet]
    GameState ship loadturn <- ask
    case getPlanetByName (loadturn ^. loadturnRst) planet of
        Just planet' ->
            modify $ alter
                (\old -> case old of
                    Just old' -> Just $ old'
                        { _shipUpdateX = Just $ planet' ^. planetX
                        , _shipUpdateY = Just $ planet' ^. planetY
                        }
                    Nothing   -> Just $ (defaultShipUpdate (ship ^. shipId)) { _shipUpdateX = Just $ planet' ^. planetX, _shipUpdateY = Just $ planet' ^. planetY }
                ) $ ShipEntity (ship ^. shipId)
        Nothing -> pure ()
        -- Do not continue the script
interpretGS (Free (Pickup amt resource next)) = do
    tell ["Pickup " <> show amt <> " " <> show resource]
    GameState ship loadturn <- ask
    case getPlanetAtShip (loadturn ^. loadturnRst) ship of
        Just planet -> do
            let (amtOnShip, amtOnPlanet) = case resource of
                    Clans    -> (ship ^. shipClans, planet ^. planetClans)
                    Mc       -> (ship ^. shipMegaCredits, planet ^. planetMegaCredits)
                    Supplies -> (ship ^. shipSupplies, planet ^. planetSupplies)
                    Neu      -> (ship ^. shipNeutronium, planet ^. planetNeutronium)
                    Dur      -> (ship ^. shipDuranium, planet ^. planetDuranium)
                    Tri      -> (ship ^. shipTritanium, planet ^. planetTritanium)
                    Mol      -> (ship ^. shipMolybdenum, planet ^. planetMolybdenum)
            -- TBD - Check how much this ship can actually hold and only transfer that
            let amtToTransfer = case amt of
                    Max        -> amtOnPlanet
                    Exact amt' -> min (amt' - amtOnShip) amtOnPlanet
            transfer ship planet resource amtOnShip amtOnPlanet amtToTransfer
            pure ()
        Nothing     -> pure ()
    interpretGS next
interpretGS (Free (DropOff amt resource next)) = do
    tell ["DropOff " <> show amt <> " " <> show resource]
    GameState ship loadturn <- ask
    case getPlanetAtShip (loadturn ^. loadturnRst) ship of
        Just planet -> do
            let (amtOnShip, amtOnPlanet) = case resource of
                    Clans    -> (ship ^. shipClans, planet ^. planetClans)
                    Mc       -> (ship ^. shipMegaCredits, planet ^. planetMegaCredits)
                    Supplies -> (ship ^. shipSupplies, planet ^. planetSupplies)
                    Neu      -> (ship ^. shipNeutronium, planet ^. planetNeutronium)
                    Dur      -> (ship ^. shipDuranium, planet ^. planetDuranium)
                    Tri      -> (ship ^. shipTritanium, planet ^. planetTritanium)
                    Mol      -> (ship ^. shipMolybdenum, planet ^. planetMolybdenum)
            let amtToTransfer = case amt of
                    Max        -> amtOnShip
                    Exact amt' -> min amt' amtOnShip
            transfer ship planet resource amtOnShip amtOnPlanet (-1 * amtToTransfer)
            pure ()
        Nothing     -> pure ()
    interpretGS next
interpretGS (Free (GetShip next)) = do
    tell ["GetShip"]
    GameState ship loadturn <- ask
    interpretGS $ next ship
interpretGS (Free (GetShipHull ship next)) = do
    tell ["GetHull " <> ship ^. shipName]
    GameState ship loadturn <- ask
    let hull = getHull (loadturn ^. loadturnRst) ship
    interpretGS $ next hull
interpretGS (Free (GetPlanets next)) = do
    tell ["GetPlanets"]
    GameState ship loadturn <- ask
    let planets = loadturn ^. loadturnRst ^. rstPlanets
    interpretGS $ next planets

transfer :: MonadState (Map EntityKey Update) m => Ship -> Planet -> Resource -> Int -> Int -> Int -> m ()
transfer ship planet resource amtOnShip amtOnPlanet amtToTransfer = do
    -- One of the horriblest things i've had to write - its aweful and i promise to do this better
    -- The api is terrible. When tranferring TO PLANETS you must set the *transfer* fields, AND NOT SEND A PLANETUPDATE
    -- but when you are transferring from a planet TO A SHIP you send both updates, but no *transfer* fields... WTF
    modify $ alter (
                \maybeExistingUpdate ->
                    let shipUpdate = case maybeExistingUpdate of
                            Just existing -> existing
                            Nothing       -> defaultShipUpdate (ship ^. shipId)
                    in Just $ alterShipUpdate shipUpdate
                ) $ ShipEntity (ship ^. shipId)

    modify $ alter (
                \maybeExistingUpdate ->
                    let planetUpdate = case maybeExistingUpdate of
                            Just existing -> existing
                            Nothing -> defaultPlanetUpdate (planet ^. planetId)
                    in Just $ alterPlanetUpdate planetUpdate
                ) $ PlanetEntity (planet ^. planetId)

    -- If the transfer is from Ship -> Planet - then we set the "transfer target" stuff correctly
    modify $ adjust (
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
                ) $ ShipEntity (ship ^. shipId)

    -- Delete the planet update if the transfer is towards the planet (I actually think now that this is only required if we
    -- are NOT THE CURRENT OWNER - need to test)
    modify $ alter (\planetUpdate -> if amtToTransfer < 0 then Nothing else planetUpdate) $ PlanetEntity (planet ^. planetId)

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

restore :: String -> GameState -> Instruction a -> Instruction a
restore location state@(GameState ship loadturn) instr =
    case instr of
        (Pure a)                   -> pure a
        (Free (FlyTo planet next)) | planet == location -> next -- found our restore point :)
                                   | otherwise -> restore location state next
        (Free (Pickup amt resource next))   -> restore location state next
        (Free (DropOff amt resource next))  -> restore location state next
        (Free (GetShip next)) -> restore location state (next ship)
        (Free (GetShipHull ship next)) -> do
            let hull = getHull (loadturn ^. loadturnRst) ship
            restore location state (next hull)
        (Free (GetPlanets next)) -> restore location state (next (loadturn ^. loadturnRst ^. rstPlanets))

runWithRestore :: String -> Instruction a -> GameState -> ([String], Map EntityKey Update)
runWithRestore location script state = do
    let restored = restore location state script
    let x = interpretGS restored
    runWithGS x state

runWithGS :: AutoM a -> GameState -> ([String], Map EntityKey Update)
runWithGS ama gs =
    let (a, state, updates) = runRWS ama gs empty
     in (updates, state)

