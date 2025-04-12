module Auto where
import           Api                    (ApiKey, LoadTurnResponse, Rst, Ship,
                                         Update (..), loadturnRst, planetId,
                                         planetMegaCredits, planetX, planetY,
                                         shipId, shipMegaCredits, update)
import           Calcs                  (getPlanetAtShip, getPlanetByName)
import           Control.Monad.Free     (Free (..), liftF)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.RWS      (RWS, modify, runRWS, tell)
import           Data.Map               (Map, alter, empty, insert)
import           Optics.Operators

type Script = Instruction ()

data GameState = GameState Ship LoadTurnResponse

type ShipId = Int

type AutoM = RWS GameState [String] (Map ShipId Update)

data Amount = Max deriving (Show)
data Resource = Mc deriving (Show)

data Instr next
    = FlyTo String next
    | Pickup Amount Resource next
    | DropOff Amount Resource next
    deriving (Functor)

type Instruction = Free Instr

flyTo :: String -> Instruction ()
flyTo planet = liftF $ FlyTo planet ()

pickup :: Amount -> Resource -> Instruction ()
pickup amt resource = liftF $ Pickup amt resource ()

dropOff :: Amount -> Resource -> Instruction ()
dropOff amt resource = liftF $ DropOff amt resource ()

interpret :: Instruction a -> [String]
interpret (Pure _)                   = []
interpret (Free (FlyTo planet next)) = ("FlyTo " <> planet) : interpret next
interpret (Free (Pickup amt resource next))   = ("Pickup " <> show amt <> " " <> show resource) : interpret next
interpret (Free (DropOff amt resource next))  = ("DropOff " <> show amt <> " " <> show resource) : interpret next

interpretIO :: Instruction a -> IO ()
interpretIO (Pure _) = pure ()
interpretIO (Free (FlyTo planet next)) = do
    putStrLn $ "FlyTo " <> planet
    interpretIO next
interpretIO (Free (Pickup amt resource next))   = do
    putStrLn $ "Pickup " <> show amt
    interpretIO next
interpretIO (Free (DropOff amt resource next))   = do
    putStrLn $ "DropOff " <> show amt
    interpretIO next

interpretGS :: Instruction a -> AutoM ()
interpretGS (Pure _) = pure ()
interpretGS (Free (FlyTo planet next)) = do
    GameState ship loadturn <- ask
    case getPlanetByName (loadturn ^. loadturnRst) planet of
        Just planet' ->
            modify $ alter
                (\old -> case old of
                    Just old' -> Just $ old'
                        { _shipUpdateX = Just $ planet' ^. planetX
                        , _shipUpdateY = Just $ planet' ^. planetY
                        }
                    Nothing   -> Just $ ShipUpdate (ship ^. shipId) (Just $ planet' ^. planetX) (Just $ planet' ^. planetY) Nothing
                ) (ship ^. shipId)
                        -- insert (ship ^. shipId)
                        --        [ShipUpdate (ship ^. shipId) (Just $ planet' ^. planetX) (Just $ planet' ^. planetY) Nothing]
                        --        state
            -- tell $ [ShipUpdate (ship ^. shipId) (Just $ planet' ^. planetX) (Just $ planet' ^. planetY) Nothing]
        Nothing -> pure ()
        -- Do not continue the script
interpretGS (Free (Pickup amt resource next)) = do
    GameState ship loadturn <- ask
    case getPlanetAtShip (loadturn ^. loadturnRst) ship of
        Just planet -> do
            let amtOnPlanet = case amt of
                    Max -> planet ^. planetMegaCredits
            let amtToPickup = case amt of
                    Max -> planet ^. planetMegaCredits
            let amtOnShip = case resource of
                    Mc -> ship ^. shipMegaCredits

            modify $ alter
                (\old -> case old of
                    Just old' -> Just $ old'
                        { _shipUpdateMegaCredits = Just $ amtOnShip + amtToPickup
                        }
                    Nothing   -> Just $ ShipUpdate (ship ^. shipId) Nothing Nothing (Just $ amtOnShip + amtToPickup)
                ) (ship ^. shipId)
            modify $ alter
                (\old -> case old of
                    Just old' -> Just $ old'
                        { _planetUpdateMegaCredits = Just $ amtOnPlanet - amtToPickup
                        }
                    Nothing   -> Just $ PlanetUpdate (planet ^. planetId) (Just $ amtOnPlanet - amtToPickup)
                ) (planet ^. planetId)
            pure ()
        Nothing     -> pure ()

    interpretGS next
interpretGS (Free (DropOff amt resource next)) = do
    GameState ship loadturn <- ask
    interpretGS next

restore :: String -> Instruction a -> Instruction a
restore location instr =
    case instr of
        (Pure a)                   -> pure a
        (Free (FlyTo planet next)) | planet == location -> next -- found our restore point :)
                                   | otherwise -> restore location next
        (Free (Pickup amt resource next))   -> restore location next
        (Free (DropOff amt resource next))  -> restore location next

runWithRestore :: String -> Instruction a -> GameState -> Map ShipId Update
runWithRestore location script state = do
    let restored = restore location script
    let x = interpretGS restored
    runWithGS x state

runWithGS :: AutoM a -> GameState -> Map ShipId Update
runWithGS ama gs =
    let (a, state, updates) = runRWS ama gs empty
     in state

