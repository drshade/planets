module Api where

import           Data.Aeson                 (FromJSON,
                                             Options (fieldLabelModifier),
                                             decode, defaultOptions,
                                             eitherDecode)
import           Data.Aeson.TH              (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as LChar8 (pack, unpack)
import           Data.Char                  (toLower)
import           Data.List                  (intercalate)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map (elems)
import           Network.HTTP.Client        (httpLbs, newManager, parseRequest,
                                             responseBody, responseStatus)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types.Status  (statusCode)
import           Optics                     (makeLenses, (^.))
import           Prelude                    hiding (error)
import qualified Prelude                    as P (error)

type Username = String
type Password = String
type ApiKey = String
type GameId = String
type Url = String

type Planets a = IO a

data ErrorResponse = ErrorResponse
    { _errorResponseSuccess :: Bool
    , _errorResponseError   :: String
    }

makeLenses ''ErrorResponse
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_errorResponse") } ''ErrorResponse

data LoginResponse = LoginResponse
    { _loginResponseApiKey :: ApiKey
    }

makeLenses ''LoginResponse
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_loginResponse") } ''LoginResponse

data Waypoint = Waypoint
    { _waypointX :: Int
    , _waypointY :: Int
    } deriving (Show)

makeLenses ''Waypoint
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_waypoint") } ''Waypoint

data Ship = Ship
    { _shipName                :: String
    , _shipId                  :: Int
    , _shipOwnerId             :: Int
    , _shipWarp                :: Int
    , _shipX                   :: Int
    , _shipY                   :: Int
    , _shipTargetX             :: Int
    , _shipTargetY             :: Int
    , _shipClans               :: Int
    , _shipNeutronium          :: Int
    , _shipTritanium           :: Int
    , _shipDuranium            :: Int
    , _shipMolybdenum          :: Int
    , _shipSupplies            :: Int
    , _shipMegaCredits         :: Int
    , _shipAmmo                :: Int
    , _shipHullId              :: Int
    , _shipEngineId            :: Int
    , _shipTransferTargetId    :: Int
    , _shipTransferTargetType  :: Int
    , _shipTransferMegaCredits :: Int
    , _shipTransferSupplies    :: Int
    , _shipTransferClans       :: Int
    , _shipTransferNeutronium  :: Int
    , _shipTransferDuranium    :: Int
    , _shipTransferTritanium   :: Int
    , _shipTransferMolybdenum  :: Int
    , _shipTransferAmmo        :: Int
    , _shipFriendlyCode        :: String
    , _shipMission             :: Int
    , _shipMission1Target      :: Int
    , _shipMission2Target      :: Int
    , _shipPodHullId           :: Int
    , _shipPodCargo            :: Int
    , _shipEnemy               :: Int
    , _shipWaypoints           :: [Waypoint]
    , _shipReadyStatus         :: Int

    } deriving (Show)

makeLenses ''Ship
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_ship") } ''Ship

data Engine = Engine
    { _engineName      :: String
    , _engineId        :: Int
    , _engineWarp1     :: Int
    , _engineWarp2     :: Int
    , _engineWarp3     :: Int
    , _engineWarp4     :: Int
    , _engineWarp5     :: Int
    , _engineWarp6     :: Int
    , _engineWarp7     :: Int
    , _engineWarp8     :: Int
    , _engineWarp9     :: Int
    , _engineTechLevel :: Int
    } deriving (Show)

makeLenses ''Engine
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_engine") } ''Engine

data Hull = Hull
    { _hullName        :: String
    , _hullId          :: Int
    , _hullCargo       :: Int
    , _hullFuelTank    :: Int
    , _hullFighterBays :: Int
    , _hullLaunchers   :: Int
    , _hullBeams       :: Int
    , _hullEngines     :: Int
    , _hullTechLevel   :: Int
    } deriving (Show)

makeLenses ''Hull
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_hull") } ''Hull

data Planet = Planet
    { _planetName                :: String
    , _planetId                  :: Int
    , _planetOwnerId             :: Int
    , _planetTemp                :: Int
    , _planetX                   :: Int
    , _planetY                   :: Int
    , _planetMines               :: Int
    , _planetFactories           :: Int
    , _planetDefense             :: Int
    , _planetTargetMines         :: Int
    , _planetTargetFactories     :: Int
    , _planetTargetDefense       :: Int
    , _planetBuiltMines          :: Int
    , _planetBuiltFactories      :: Int
    , _planetBuiltDefense        :: Int
    , _planetMegaCredits         :: Int
    , _planetSupplies            :: Int
    , _planetSuppliesSold        :: Int
    , _planetNeutronium          :: Int
    , _planetMolybdenum          :: Int
    , _planetDuranium            :: Int
    , _planetTritanium           :: Int
    , _planetGroundNeutronium    :: Int
    , _planetGroundMolybdenum    :: Int
    , _planetGroundDuranium      :: Int
    , _planetGroundTritanium     :: Int
    , _planetDensityNeutronium   :: Int
    , _planetDensityMolybdenum   :: Int
    , _planetDensityDuranium     :: Int
    , _planetDensityTritanium    :: Int
    , _planetTotalNeutronium     :: Int
    , _planetTotalMolybdenum     :: Int
    , _planetTotalDuranium       :: Int
    , _planetTotalTritanium      :: Int
    , _planetCheckNeutronium     :: Int
    , _planetCheckMolybdenum     :: Int
    , _planetCheckDuranium       :: Int
    , _planetCheckTritanium      :: Int
    , _planetCheckMegaCredits    :: Int
    , _planetCheckSupplies       :: Int
    , _planetClans               :: Int
    , _planetColonistTaxRate     :: Int
    , _planetColonistHappyPoints :: Int
    , _planetColHappyChange      :: Int
    , _planetColChange           :: Int
    , _planetNativeClans         :: Int
    , _planetNativeType          :: Int
    , _planetNativeTaxRate       :: Int
    , _planetNativeTaxValue      :: Int
    , _planetNativeGovernment    :: Int
    , _planetNativeHappyPoints   :: Int
    , _planetNativeHappyChange   :: Int
    , _planetFriendlyCode        :: String
    , _planetBuildingStarbase    :: Bool
    , _planetReadyStatus         :: Int
    , _planetPodHullId           :: Int
    , _planetPodCargo            :: Int
    , _planetPodSpeed            :: Int
    , _planetTargetX             :: Int
    , _planetTargetY             :: Int
    , _planetDevelopmentLevel    :: Int
    } deriving (Show)

makeLenses ''Planet
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_planet") } ''Planet

data Game = Game
    { _gameId   :: Int
    , _gameName :: String
    , _gameTurn :: Int
    } deriving (Show)

makeLenses ''Game
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_game") } ''Game

data Player = Player
    { _playerId       :: Int
    , _playerUsername :: String
    , _playerRaceId   :: Int
    } deriving (Show)

makeLenses ''Player
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_player") } ''Player

data Starbase = Starbase
    { _starbaseId              :: Int
    , _starbasePlanetId        :: Int
    , _starbaseEngineTechLevel :: Int
    , _starbaseHullTechLevel   :: Int
    , _starbaseBeamTechLevel   :: Int
    , _starbaseTorpTechLevel   :: Int
    , _starbaseFighters        :: Int
    } deriving (Show)

makeLenses ''Starbase
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_starbase") } ''Starbase

data Rst = Rst
    { _rstGame      :: Game
    , _rstPlayer    :: Player
    , _rstPlayers   :: [Player]
    , _rstShips     :: [Ship]
    , _rstPlanets   :: [Planet]
    , _rstHulls     :: [Hull]
    , _rstEngines   :: [Engine]
    , _rstStarbases :: [Starbase]
    } deriving (Show)

makeLenses ''Rst
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_rst") } ''Rst

data LoadTurnResponse = LoadTurnResponse
    { _loadturnCanMessage :: Maybe Bool
    , _loadturnRst        :: Rst
    , _loadturnSaveKey    :: String
    , _loadturnIsPremium  :: Bool
    } deriving (Show)

makeLenses ''LoadTurnResponse
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_loadturn") } ''LoadTurnResponse

fetch :: FromJSON a => Url -> Planets a
fetch url = do
    let apiUrl = "http://api.planets.nu"
    req <- parseRequest $ apiUrl ++ url
    manager <- newManager tlsManagerSettings
    resp <- httpLbs req manager
    if statusCode (responseStatus resp) /= 200
        then do
            -- I don't think we error get non 200 responses, but will deal with it later!
            let body :: String = LChar8.unpack $ responseBody resp
            P.error $ "Non-200 response returned... implement this! -> " ++ body
        else do
            let body :: String = LChar8.unpack $ responseBody resp
            -- putStrLn $ "Response: " ++ body
            -- First check if it's an error by decoding the response
            let errorResponse = decode (LChar8.pack body) :: Maybe ErrorResponse
            case errorResponse of
                Just err -> do
                    P.error $ "Error: " ++ err ^. errorResponseError
                Nothing -> do
                    -- Its not an error! Parse the type we expect, or error
                    let x = eitherDecode (LChar8.pack body)
                    case x of
                        Right a  -> pure a
                        Left err -> P.error $ "Failed to parse response: " ++ body ++ "\n-> " ++ err

login :: Username -> Password -> Planets ApiKey
login username password = do
    let url = "/account/login?username=" ++ username ++ "&password=" ++ password
    res :: LoginResponse <- fetch url
    pure $ res ^. loginResponseApiKey

currentTurn :: ApiKey -> GameId -> Planets LoadTurnResponse
currentTurn apikey gameid = do
    let url = "/game/loadturn?apikey=" ++ apikey ++ "&gameid=" ++ gameid ++ "&forsave=true"
    res :: LoadTurnResponse <- fetch url
    pure res


{-
   Id:::1
|||Name:::Medium+Deep+Space+Freighter
|||Neutronium:::95
|||Duranium:::0
|||Tritanium:::0
|||Molybdenum:::0
|||MegaCredits:::0
|||Supplies:::0
|||Clans:::50
|||Ammo:::0
|||TransferNeutronium:::0
|||TransferDuranium:::0
|||TransferTritanium:::0
|||TransferMolybdenum:::0
|||TransferMegaCredits:::0
|||TransferSupplies:::0
|||TransferClans:::0
|||TransferAmmo:::0
|||TransferTargetId:::0
|||TransferTargetType:::0
|||TargetX:::2089
|||TargetY:::2016
|||FriendlyCode:::vcl
|||Warp:::9
|||Mission:::0
|||Mission1Target:::0
|||Mission2Target:::0
|||PodHullId:::0
|||PodCargo:::0
|||Enemy:::0
|||Waypoints:::2037,2015:2023,2052:1999,2025:2017,1997:
|||ReadyStatus:::0
-}

data ShipUpdate
    = ShipUpdate
        { _shipUpdateShipId              :: Int
        , _shipUpdateName                :: String
        , _shipUpdateNeutronium          :: Int
        , _shipUpdateDuranium            :: Int
        , _shipUpdateTritanium           :: Int
        , _shipUpdateMolybdenum          :: Int
        , _shipUpdateMegaCredits         :: Int
        , _shipUpdateSupplies            :: Int
        , _shipUpdateClans               :: Int
        , _shipUpdateAmmo                :: Int
        , _shipUpdateTransferNeutronium  :: Int
        , _shipUpdateTransferDuranium    :: Int
        , _shipUpdateTransferTritanium   :: Int
        , _shipUpdateTransferMolybdenum  :: Int
        , _shipUpdateTransferMegaCredits :: Int
        , _shipUpdateTransferSupplies    :: Int
        , _shipUpdateTransferClans       :: Int
        , _shipUpdateTransferAmmo        :: Int
        , _shipUpdateTransferTargetId    :: Int
        , _shipUpdateTransferTargetType  :: Int
        , _shipUpdateTargetX             :: Int
        , _shipUpdateTargetY             :: Int
        , _shipUpdateFriendlyCode        :: String
        , _shipUpdateWarp                :: Int
        , _shipUpdateMission             :: Int
        , _shipUpdateMission1Target      :: Int
        , _shipUpdateMission2Target      :: Int
        , _shipUpdatePodHullId           :: Int
        , _shipUpdatePodCargo            :: Int
        , _shipUpdateEnemy               :: Int
        , _shipUpdateWaypoints           :: [Waypoint]
        , _shipUpdateReadyStatus         :: Int
        }

makeLenses ''ShipUpdate

{-
Id:::4
|||FriendlyCode:::563
|||Mines:::37
|||Factories:::54
|||Defense:::12
|||TargetMines:::0
|||TargetFactories:::0
|||TargetDefense:::0
|||BuiltMines:::0
|||BuiltFactories:::0
|||BuiltDefense:::0
|||MegaCredits:::11209
|||Supplies:::2304
|||SuppliesSold:::0
|||Neutronium:::1378
|||Duranium:::739
|||Tritanium:::1119
|||Molybdenum:::2356
|||Clans:::15997
|||ColonistTaxRate:::8
|||NativeTaxRate:::0
|||BuildingStarbase:::false
|||NativeHappyChange:::4
|||ColHappyChange:::2
|||ColChange:::0
|||ReadyStatus:::0
|||PodHullId:::0
|||PodCargo:::0
|||PodSpeed:::0
|||NativeClans:::0
|||TargetX:::2089
|||TargetY:::2016
|||DevelopmentLevel:::0
-}

data PlanetUpdate
    = PlanetUpdate
        { _planetUpdatePlanetId          :: Int
        , _planetUpdateFriendlyCode      :: String
        , _planetUpdateMines             :: Int
        , _planetUpdateFactories         :: Int
        , _planetUpdateDefense           :: Int
        , _planetUpdateTargetMines       :: Int
        , _planetUpdateTargetFactories   :: Int
        , _planetUpdateTargetDefense     :: Int
        , _planetUpdateBuiltMines        :: Int
        , _planetUpdateBuiltFactories    :: Int
        , _planetUpdateBuiltDefense      :: Int
        , _planetUpdateMegaCredits       :: Int
        , _planetUpdateSupplies          :: Int
        , _planetUpdateSuppliesSold      :: Int
        , _planetUpdateNeutronium        :: Int
        , _planetUpdateDuranium          :: Int
        , _planetUpdateTritanium         :: Int
        , _planetUpdateMolybdenum        :: Int
        , _planetUpdateClans             :: Int
        , _planetUpdateColonistTaxRate   :: Int
        , _planetUpdateNativeTaxRate     :: Int
        , _planetUpdateBuildingStarbase  :: Bool
        , _planetUpdateNativeHappyChange :: Int
        , _planetUpdateColHappyChange    :: Int
        , _planetUpdateColChange         :: Int
        , _planetUpdateReadyStatus       :: Int
        , _planetUpdatePodHullId         :: Int
        , _planetUpdatePodCargo          :: Int
        , _planetUpdatePodSpeed          :: Int
        , _planetUpdateNativeClans       :: Int
        , _planetUpdateTargetX           :: Int
        , _planetUpdateTargetY           :: Int
        , _planetUpdateDevelopmentLevel  :: Int
        }

makeLenses ''PlanetUpdate

data TransferTargetType = PlanetTransferTarget

transferTargetType :: TransferTargetType -> Int
transferTargetType PlanetTransferTarget = 1

instance Show ShipUpdate where
    show (ShipUpdate id' name neu dur tri mol mc sup clans ammo tneu tdur ttri tmol tmc tsup tclans tammo ttargetid ttargettype targetx targety fc warp mission m1target m2target podhullid podcargo enemy waypoints readystatus) =
        "Ship" <> show id' <> "=Id:::" <> show id'
        <> build "Name" name
        <> build "Neutronium" neu
        <> build "Duranium" dur
        <> build "Tritanium" tri
        <> build "Molybdenum" mol
        <> build "MegaCredits" mc
        <> build "Supplies" sup
        <> build "Clans" clans
        <> build "Ammo" ammo
        <> build "TransferNeutronium" tneu
        <> build "TransferDuranium" tdur
        <> build "TransferTritanium" ttri
        <> build "TransferMolybdenum" tmol
        <> build "TransferMegaCredits" tmc
        <> build "TransferSupplies" tsup
        <> build "TransferClans" tclans
        <> build "TransferAmmo" tammo
        <> build "TransferTargetId" ttargetid
        <> build "TransferTargetType" ttargettype
        <> build "TargetX" targetx
        <> build "TargetY" targety
        <> buildString "FriendlyCode" fc
        <> build "Warp" warp
        <> build "Mission" mission
        <> build "Mission1Target" m1target
        <> build "Mission2Target" m2target
        <> build "PodHullId" podhullid
        <> build "PodCargo" podcargo
        <> build "Enemy" enemy
        <> buildString "Waypoints" (intercalate "" $ (\(Waypoint x y) -> show x <> "," <> show y <> ":") <$> waypoints)
        <> build "ReadyStatus" readystatus
        where
            build key v = "|||" <> key <> ":::" <> show v
            buildString key v = "|||" <> key <> ":::" <> v


instance Show PlanetUpdate where
    show (PlanetUpdate id' fc mines factories defense tmines tfactories tdefense bmines bfactories bdefense mc sup supsold neu dur tri mol clans coltaxrate nativetaxrate buildingstarbase nativehappychange colhappychange colchange readystatus podhullid podcargo podspeed nativeclans targetx targety developmentlevel) =
        "Planet" <> show id' <> "=Id:::" <> show id'
        <> build "Clans" clans
        <> buildString "FriendlyCode" fc
        <> build "Mines" mines
        <> build "Factories" factories
        <> build "Defense" defense
        <> build "TargetMines" tmines
        <> build "TargetFactories" tfactories
        <> build "TargetDefense" tdefense
        <> build "BuiltMines" bmines
        <> build "BuiltFactories" bfactories
        <> build "BuiltDefense" bdefense
        <> build "MegaCredits" mc
        <> build "Supplies" sup
        <> build "SuppliesSold" supsold
        <> build "Neutronium" neu
        <> build "Duranium" dur
        <> build "Tritanium" tri
        <> build "Molybdenum" mol
        <> build "Clans" clans
        <> build "ColonistTaxRate" coltaxrate
        <> build "NativeTaxRate" nativetaxrate
        <> build "BuildingStarbase" buildingstarbase
        <> build "NativeHappyChange" nativehappychange
        <> build "ColHappyChange" colhappychange
        <> build "ColChange" colchange
        <> build "ReadyStatus" readystatus
        <> build "PodHullId" podhullid
        <> build "PodCargo" podcargo
        <> build "PodSpeed" podspeed
        <> build "NativeClans" nativeclans
        <> build "TargetX" targetx
        <> build "TargetY" targety
        <> build "DevelopmentLevel" developmentlevel
        where
            build key x = "|||" <> key <> ":::" <> show x
            buildString key v = "|||" <> key <> ":::" <> v

update :: ApiKey -> LoadTurnResponse -> Map Int ShipUpdate -> Map Int PlanetUpdate -> Planets ()
update apikey loadturn shipUpdates planetUpdates = do
    let params = "?gameid=" <> show (loadturn ^. loadturnRst ^. rstGame ^. gameId)
                 <> "&playerid=" <> show (loadturn ^. loadturnRst ^. rstPlayer ^. playerId)
                 <> "&turn=" <> show (loadturn ^. loadturnRst ^. rstGame ^. gameTurn)
                 <> "&version=4"
                 <> "&savekey=" <> (loadturn ^. loadturnSaveKey)
                 <> "&apikey=" <> apikey
                 <> "&saveindex=2"
                 --  <> "&Ship7=Id:::7|||Name:::LARGE+DEEP+SPACE+FREIGHTER|||Neutronium:::100|||Duranium:::0|||Tritanium:::0|||Molybdenum:::0|||MegaCredits:::110|||Supplies:::0|||Clans:::0|||Ammo:::0|||TransferNeutronium:::0|||TransferDuranium:::0|||TransferTritanium:::0|||TransferMolybdenum:::0|||TransferMegaCredits:::0|||TransferSupplies:::0|||TransferClans:::0|||TransferAmmo:::0|||TransferTargetId:::0|||TransferTargetType:::0|||TargetX:::1690|||TargetY:::2030|||FriendlyCode:::svw|||Warp:::9|||Mission:::0|||Mission1Target:::0|||Mission2Target:::0|||PodHullId:::0|||PodCargo:::0|||Enemy:::0|||Waypoints:::|||ReadyStatus:::0&Planet16=Id:::16|||FriendlyCode:::316|||Mines:::355|||Factories:::255|||Defense:::20|||TargetMines:::0|||TargetFactories:::0|||TargetDefense:::0|||BuiltMines:::0|||BuiltFactories:::0|||BuiltDefense:::0|||MegaCredits:::2725|||Supplies:::502|||SuppliesSold:::0|||Neutronium:::810|||Molybdenum:::1384|||Duranium:::236|||Tritanium:::868|||Clans:::24150|||ColonistTaxRate:::7|||NativeTaxRate:::0|||BuildingStarbase:::false|||NativeHappyChange:::1|||ColHappyChange:::0|||ColChange:::0|||ReadyStatus:::0|||PodHullId:::0|||PodCargo:::0|||PodSpeed:::0|||NativeClans:::0|||TargetX:::1926|||TargetY:::1894|||DevelopmentLevel:::0&Starbase1=Id:::1|||Fighters:::20|||Defense:::100|||BuiltFighters:::0|||BuiltDefense:::0|||HullTechLevel:::6|||EngineTechLevel:::10|||BeamTechLevel:::1|||TorpTechLevel:::1|||HullTechUp:::0|||EngineTechUp:::0|||BeamTechUp:::0|||TorpTechUp:::0|||Mission:::0|||Mission1Target:::0|||ShipMission:::0|||TargetShipId:::0|||BuildHullId:::0|||BuildEngineId:::0|||BuildBeamId:::0|||BuildTorpedoId:::0|||BuildBeamCount:::0|||BuildTorpCount:::0|||IsBuilding:::false|||ReadyStatus:::0"
                 --            "Id:::1|||Name:::Medium+Deep+Space+Freighter|||Neutronium:::75|||Duranium:::0|||Tritanium:::0|||Molybdenum:::0|||MegaCredits:::0|||Supplies:::0|||Clans:::0|||Ammo:::0|||TransferNeutronium:::0|||TransferDuranium:::0|||TransferTritanium:::0|||TransferMolybdenum:::0|||TransferMegaCredits:::400|||TransferSupplies:::100|||TransferClans:::100|||TransferAmmo:::0|||TransferTargetId:::7|||TransferTargetType:::1|||TargetX:::1924|||TargetY:::1914|||FriendlyCode:::sdt|||Warp:::9|||Mission:::0|||Mission1Target:::0|||Mission2Target:::0|||PodHullId:::0|||PodCargo:::0|||Enemy:::0|||Waypoints:::|||ReadyStatus:::0""
                 <> (if 0 < (length $ Map.elems shipUpdates)
                        then "&" <> intercalate "&" (show <$> Map.elems shipUpdates)
                        else "")
                 <> (if 0 < (length $ Map.elems planetUpdates)
                        then "&" <> intercalate "&" (show <$> Map.elems planetUpdates)
                        else "")
                 <> "&keycount=" <> show (8 + length shipUpdates + length planetUpdates)
    putStrLn params
    let apiUrl = "http://api.planets.nu/game/save"
    req <- parseRequest $ apiUrl ++ params
    manager <- newManager tlsManagerSettings
    resp <- httpLbs req manager
    if statusCode (responseStatus resp) /= 200
        then do
            -- I don't think we error get non 200 responses, but will deal with it later!
            let body :: String = LChar8.unpack $ responseBody resp
            P.error $ "Non-200 response returned... implement this! -> " ++ body
        else do
            let body :: String = LChar8.unpack $ responseBody resp
            -- putStrLn $ "Response: " ++ body
            -- First check if it's an error by decoding the response
            let errorResponse = decode (LChar8.pack body) :: Maybe ErrorResponse
            case errorResponse of
                Just err -> do
                    P.error $ "Error: " ++ err ^. errorResponseError
                Nothing -> do
                    -- Its not an error! Parse the type we expect, or error
                    let x = decode (LChar8.pack body)
                    case x of
                        Just a  -> pure a
                        Nothing -> P.error $ "Failed to parse response: " ++ body
