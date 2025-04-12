module Auto where
import           Api                    (ApiKey, LoadTurnResponse, Rst, Ship,
                                         Update (..), loadturnRst, planetId,
                                         planetMegaCredits, planetX, planetY,
                                         shipId, shipMegaCredits, update)
import           Calcs                  (getPlanetAtShip, getPlanetByName)
import           Control.Monad.Free     (Free (..), liftF)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.RWS      (RWS, runRWS, tell)
import           Optics.Operators

type Script = Instruction ()

data GameState = GameState Ship LoadTurnResponse

type AutoM = RWS GameState [Update] ()

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
        Just planet' -> tell $ [ShipUpdate (ship ^. shipId) (Just $ planet' ^. planetX) (Just $ planet' ^. planetY) Nothing]
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
            tell $ [ShipUpdate (ship ^. shipId) Nothing Nothing (Just $ amtOnShip + amtToPickup)]
            tell $ [PlanetUpdate (planet ^. planetId) (Just $ amtOnPlanet - amtToPickup)]
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

runWithRestore :: String -> Instruction a -> GameState -> [Update]
runWithRestore location script state = do
    let restored = restore location script
    let x = interpretGS restored
    runWithGS x state

runWithGS :: AutoM a -> GameState -> [Update]
runWithGS ama gs =
    let (a, _state, updates) = runRWS ama gs ()
     in updates

