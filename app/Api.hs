module Api where

import           Data.Aeson                 (FromJSON,
                                             Options (fieldLabelModifier),
                                             decode, defaultOptions)
import           Data.Aeson.TH              (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as LChar8 (pack, unpack)
import           Data.Char                  (toLower)
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

data Ship = Ship
    { _shipName        :: String
    , _shipId          :: Int
    , _shipOwnerId     :: Int
    , _shipwarp        :: Int
    , _shipX           :: Int
    , _shipY           :: Int
    , _shipTargetX     :: Int
    , _shipTargetY     :: Int
    , _shipClans       :: Int
    , _shipNeutronium  :: Int
    , _shipTritanium   :: Int
    , _shipDuranium    :: Int
    , _shipMolybdenum  :: Int
    , _shipSupplies    :: Int
    , _shipMegaCredits :: Int
    , _shipHullId      :: Int
    , _shipEngineId    :: Int
    , _shipAmmo        :: Int
    } deriving (Show)

makeLenses ''Ship
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_ship") } ''Ship

data Engine = Engine
    { _engineName  :: String
    , _engineId    :: Int
    , _engineWarp1 :: Int
    , _engineWarp2 :: Int
    , _engineWarp3 :: Int
    , _engineWarp4 :: Int
    , _engineWarp5 :: Int
    , _engineWarp6 :: Int
    , _engineWarp7 :: Int
    , _engineWarp8 :: Int
    , _engineWarp9 :: Int
    } deriving (Show)

makeLenses ''Engine
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_engine") } ''Engine

data Hull = Hull
    { _hullName  :: String
    , _hullId    :: Int
    , _hullCargo :: Int
    } deriving (Show)

makeLenses ''Hull
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_hull") } ''Hull

data Planet = Planet
    { _planetName              :: String
    , _planetId                :: Int
    , _planetOwnerId           :: Int
    , _planetX                 :: Int
    , _planetY                 :: Int
    , _planetMines             :: Int
    , _planetFactories         :: Int
    , _planetDefense           :: Int
    , _planetMegaCredits       :: Int
    , _planetSupplies          :: Int
    , _planetSuppliesSold      :: Int
    , _planetNeutronium        :: Int
    , _planetMolybdenum        :: Int
    , _planetDuranium          :: Int
    , _planetTritanium         :: Int
    , _planetGroundNeutronium  :: Int
    , _planetGroundMolybdenum  :: Int
    , _planetGroundDuranium    :: Int
    , _planetGroundTritanium   :: Int
    , _planetDensityNeutronium :: Int
    , _planetDensityMolybdenum :: Int
    , _planetDensityDuranium   :: Int
    , _planetDensityTritanium  :: Int
    , _planetTotalNeutronium   :: Int
    , _planetTotalMolybdenum   :: Int
    , _planetTotalDuranium     :: Int
    , _planetTotalTritanium    :: Int
    , _planetCheckNeutronium   :: Int
    , _planetCheckMolybdenum   :: Int
    , _planetCheckDuranium     :: Int
    , _planetCheckTritanium    :: Int
    , _planetCheckMegaCredits  :: Int
    , _planetCheckSupplies     :: Int
    , _planetClans             :: Int
    , _planetColonistTaxRate   :: Int
    , _planetNativeClans       :: Int
    , _planetNativeType        :: Int
    , _planetNativeTaxRate     :: Int
    , _planetNativeTaxValue    :: Int
    , _planetNativeGovernment  :: Int
    } deriving (Show)

makeLenses ''Planet
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_planet") } ''Planet

data Player = Player
    { _playerId       :: Int
    , _playerUsername :: String
    } deriving (Show)

makeLenses ''Player
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_player") } ''Player

data Rst = Rst
    { _rstPlayer  :: Player
    , _rstShips   :: [Ship]
    , _rstPlanets :: [Planet]
    , _rstHulls   :: [Hull]
    , _rstEngines :: [Engine]
    } deriving (Show)

makeLenses ''Rst
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length "_rst") } ''Rst

data LoadTurnResponse = LoadTurnResponse
    { _loadturnCanMessage :: Bool
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
                    let x = decode (LChar8.pack body)
                    case x of
                        Just a  -> pure a
                        Nothing -> P.error $ "Failed to parse response: " ++ body

login :: Username -> Password -> Planets ApiKey
login username password = do
    let url = "/account/login?username=" ++ username ++ "&password=" ++ password
    res :: LoginResponse <- fetch url
    pure $ res ^. loginResponseApiKey

currentTurn :: ApiKey -> GameId -> Planets LoadTurnResponse
currentTurn apiKey gameId = do
    let url = "/game/loadturn?apikey=" ++ apiKey ++ "&gameid=" ++ gameId ++ "&forsave=true"
    res :: LoadTurnResponse <- fetch url
    pure res
