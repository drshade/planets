module Model where

import qualified Api

import           Data.Map    (Map)
import qualified Data.Map    as Map (fromList, lookup)
import           Data.Maybe  (fromMaybe, isJust, listToMaybe)
import           Data.Tuple  (swap)
import           Optics      (makeLenses, (^.))
import           Text.Printf (printf)

type GameId = Int
type PlayerId = Int
type ShipId = Int
type PlanetId = Int

data Amount = Max | Exact Int deriving (Show)
data Resource = Mc | Supplies | Clans | Neu | Dur | Tri | Mol deriving (Show)

data Minerals = Minerals
    { _mineralsNeutronium :: Int
    , _mineralsMolybdenum :: Int
    , _mineralsDuranium   :: Int
    , _mineralsTritanium  :: Int
    }

makeLenses ''Minerals

instance Show Minerals where
    show :: Minerals -> String
    show m =
            "Ne: " ++ pad (m ^. mineralsNeutronium) ++ ", "
            ++ "Du: " ++ pad (m ^. mineralsDuranium) ++ ", "
            ++ "Tr: " ++ pad (m ^. mineralsTritanium) ++ ", "
            ++ "Mo: " ++ pad (m ^. mineralsMolybdenum)
            where pad = printf "%4d"

data Resources = Resources
    { _resourcesMegaCredits :: Int
    , _resourcesSupplies    :: Int
    , _resourcesClans       :: Int
    , _resourcesMinerals    :: Minerals
    }

makeLenses ''Resources

instance Show Resources where
    show :: Resources -> String
    show r =
        "[" ++ "Mc: " ++ pad (r ^. resourcesMegaCredits) ++ ", "
            ++ "Sp: " ++ pad (r ^. resourcesSupplies) ++ ", "
            ++ show (r ^. resourcesMinerals)
            ++
        "]"
        where pad = printf "%4d"

data Game = Game
    { _gameId   :: GameId
    , _gameName :: String
    , _gameTurn :: Int
    } deriving (Show)

data Gamestate = Gamestate
    { _gamestateGame     :: Game
    , _gamestatePlayer   :: Player
    , _gamestatePlayers  :: [Player]
    , _gamestateShips    :: [Ship]
    , _gamestatePlanets  :: [Planet]
    -- Escape parachute (maybe one day remove this if unnecessary?)
    , _gamestateLoadTurn :: Api.LoadTurnResponse
    } deriving (Show)

data Player = Player
    { _playerId       :: PlayerId
    , _playerUsername :: String
    , _playerRace     :: Race
    } deriving (Show)

data Position = Position
    { _positionX :: Int
    , _positionY :: Int
    } deriving (Show)

data Ship = Ship
    { _shipName           :: String
    , _shipId             :: ShipId
    , _shipOwnerId        :: Int
    , _shipWarp           :: Int
    , _shipPosition       :: Position
    , _shipTargetPosition :: Position
    , _shipResources      :: Resources
    , _shipAmmo           :: Int
    , _shipHull           :: Hull
    , _shipEngine         :: Maybe Engine -- may not have an engine if we are an enemy ship (ie not known)
    } deriving (Show)

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

data Engine = Engine
    { _engineName      :: String
    , _engineId        :: Int
    , _engineWarp      :: Map Int Int
    , _engineTechLevel :: Int
    } deriving (Show)

data Starbase = Starbase
    { _starbaseId               :: Int
    , _starbaseEngineTechLevel  :: Int
    , _starbaseBeamTechLevel    :: Int
    , _starbaseHullTechLevel    :: Int
    , _starbaseTorpedoTechLevel :: Int
    , _starbaseFighters         :: Int
    } deriving (Show)

data Planet = Planet
    { _planetName                  :: String
    , _planetId                    :: PlanetId
    , _planetOwnerId               :: Int
    , _planetTemperature           :: Int
    , _planetPosition              :: Position
    , _planetMines                 :: Int
    , _planetFactories             :: Int
    , _planetDefenses              :: Int
    , _planetResources             :: Resources
    , _planetGroundMinerals        :: Minerals
    , _planetDensityMinerals       :: Minerals
    , _planetTotalMinerals         :: Minerals
    , _planetCheckMinerals         :: Minerals
    , _planetColonistTaxRate       :: Int
    , _planetColonistHappiness     :: Int
    , _planetColonistHappinessRate :: Int
    , _planetNativeClans           :: Int
    , _planetNativeType            :: NativeType
    , _planetNativeGovernment      :: GovtType
    , _planetNativeTaxRate         :: Int
    , _planetNativeTaxValue        :: Int
    , _planetNativeHappiness       :: Int
    , _planetNativeHappinessRate   :: Int
    , _planetStarbase              :: Maybe Starbase
    } deriving (Show)

data Race
    = Feds | Lizards | Birds | Hoards | Privateers | Cyborgs | Crystals
    | Empire | Robots | Rebels | Colonies | Plague
    deriving (Show, Eq, Ord)

raceEnumMap :: [(Race, Int)]
raceEnumMap =
    [ (Feds, 1), (Lizards, 2), (Birds, 3), (Hoards, 4), (Privateers, 5), (Cyborgs, 6), (Crystals, 7)
    , (Empire, 8), (Robots, 9), (Rebels, 10), (Colonies, 11), (Plague, 12)
    ]

instance Enum Race where
    toEnum :: Int -> Race
    toEnum r = fromMaybe Feds $ Map.lookup r $ Map.fromList $ swap <$> raceEnumMap
    fromEnum :: Race -> Int
    fromEnum i = fromMaybe 0 $ Map.lookup i $ Map.fromList raceEnumMap

data NativeType
    = None | Humanoid | Bovinoid | Reptillian | Avian | Amorphous | Insectoid
    | Amphibian | Ghipsoldal | Siliconoid | Botanical | Unknown
    deriving (Show, Eq, Ord)

nativeTypeEnumMap :: [(NativeType, Int)]
nativeTypeEnumMap =
    [ (None, 0), (Humanoid, 1), (Bovinoid, 2), (Reptillian, 3), (Avian, 4), (Amorphous, 5)
    , (Insectoid, 6), (Amphibian, 7), (Ghipsoldal, 8), (Siliconoid, 9), (Botanical, 10)
    , (Unknown, 11)
    ]

instance Enum NativeType where
    toEnum :: Int -> NativeType
    toEnum r = fromMaybe None $ Map.lookup r $ Map.fromList $ swap <$> nativeTypeEnumMap
    fromEnum :: NativeType -> Int
    fromEnum i = fromMaybe 0 $ Map.lookup i $ Map.fromList nativeTypeEnumMap

data GovtType
    = Anarchy | PreTribal | EarlyTribal | Tribal | Feudal | Monarchy | Representative
    | Participatory | Unity
    deriving (Show, Eq, Ord)

govtTypeEnumMap :: [(GovtType, Int)]
govtTypeEnumMap =
    [ (Anarchy, 0), (PreTribal, 1), (EarlyTribal, 2), (Tribal, 3), (Feudal, 4)
    , (Monarchy, 5), (Representative, 6), (Participatory, 7), (Unity, 8)
    ]

instance Enum GovtType where
    toEnum :: Int -> GovtType
    toEnum r = fromMaybe Anarchy $ Map.lookup r $ Map.fromList $ swap <$> govtTypeEnumMap
    fromEnum :: GovtType -> Int
    fromEnum i = fromMaybe 0 $ Map.lookup i $ Map.fromList govtTypeEnumMap

makeLenses ''Gamestate
makeLenses ''Game
makeLenses ''Player
makeLenses ''Position
makeLenses ''Ship
makeLenses ''Hull
makeLenses ''Engine
makeLenses ''Planet

class HasPosition a where
    position :: a -> (Int, Int)

instance HasPosition Planet where
    position :: Planet -> (Int, Int)
    position p = (p ^. planetPosition ^. positionX, p ^. planetPosition ^. positionY)

instance HasPosition Ship where
    position :: Ship -> (Int, Int)
    position s = (s ^. shipPosition ^. positionX, s ^. shipPosition ^. positionY)

class HasResources a where
    resources :: a -> Resources

instance Semigroup Resources where
  (<>) :: Resources -> Resources -> Resources
  r1 <> r2 = Resources
    { _resourcesMegaCredits = r1 ^. resourcesMegaCredits + r2 ^. resourcesMegaCredits
    , _resourcesSupplies    = r1 ^. resourcesSupplies    + r2 ^. resourcesSupplies
    , _resourcesClans       = r1 ^. resourcesClans       + r2 ^. resourcesClans
    , _resourcesMinerals    = Minerals
        { _mineralsNeutronium = r1 ^. resourcesMinerals ^. mineralsNeutronium + r2 ^. resourcesMinerals ^. mineralsNeutronium
        , _mineralsMolybdenum = r1 ^. resourcesMinerals ^. mineralsMolybdenum + r2 ^. resourcesMinerals ^. mineralsMolybdenum
        , _mineralsDuranium   = r1 ^. resourcesMinerals ^. mineralsDuranium   + r2 ^. resourcesMinerals ^. mineralsDuranium
        , _mineralsTritanium  = r1 ^. resourcesMinerals ^. mineralsTritanium  + r2 ^. resourcesMinerals ^. mineralsTritanium
        }
    }

instance Monoid Resources where
  mempty :: Resources
  mempty = Resources 0 0 0 (Minerals 0 0 0 0)
  mappend :: Resources -> Resources -> Resources
  mappend = (<>)

instance HasResources Planet where
    resources :: Planet -> Resources
    resources p =
        Resources
            { _resourcesMegaCredits = p ^. planetResources ^. resourcesMegaCredits
            , _resourcesSupplies    = p ^. planetResources ^. resourcesSupplies
            , _resourcesClans       = p ^. planetResources ^. resourcesClans
            , _resourcesMinerals    = Minerals
                { _mineralsNeutronium = p ^. planetResources ^. resourcesMinerals ^. mineralsNeutronium
                , _mineralsMolybdenum = p ^. planetResources ^. resourcesMinerals ^. mineralsMolybdenum
                , _mineralsDuranium   = p ^. planetResources ^. resourcesMinerals ^. mineralsDuranium
                , _mineralsTritanium  = p ^. planetResources ^. resourcesMinerals ^. mineralsTritanium
                }
            }

instance HasResources Ship where
    resources :: Ship -> Resources
    resources s =
        Resources
            { _resourcesMegaCredits = s ^. shipResources ^. resourcesMegaCredits
            , _resourcesSupplies    = s ^. shipResources ^. resourcesSupplies
            , _resourcesClans       = s ^. shipResources ^. resourcesClans
            , _resourcesMinerals    = Minerals
                { _mineralsNeutronium = s ^. shipResources ^. resourcesMinerals ^. mineralsNeutronium
                , _mineralsMolybdenum = s ^. shipResources ^. resourcesMinerals ^. mineralsMolybdenum
                , _mineralsDuranium   = s ^. shipResources ^. resourcesMinerals ^. mineralsDuranium
                , _mineralsTritanium  = s ^. shipResources ^. resourcesMinerals ^. mineralsTritanium
                }
            }

fromLoadTurnResponse :: Api.LoadTurnResponse -> Gamestate
fromLoadTurnResponse loadturn =
    Gamestate
        { _gamestateGame = Game
            { _gameId   = loadturn ^. Api.loadturnRst ^. Api.rstGame ^. Api.gameId
            , _gameName = loadturn ^. Api.loadturnRst ^. Api.rstGame ^. Api.gameName
            , _gameTurn = loadturn ^. Api.loadturnRst ^. Api.rstGame ^. Api.gameTurn
            }
        , _gamestatePlayer = Player
            { _playerId       = loadturn ^. Api.loadturnRst ^. Api.rstPlayer ^. Api.playerId
            , _playerUsername = loadturn ^. Api.loadturnRst ^. Api.rstPlayer ^. Api.playerUsername
            , _playerRace     = toEnum $ loadturn ^. Api.loadturnRst ^. Api.rstPlayer ^. Api.playerRaceId
            }
        , _gamestatePlayers =
            ( \p -> Player
                { _playerId       = p ^. Api.playerId
                , _playerUsername = p ^. Api.playerUsername
                , _playerRace     = toEnum $ p ^. Api.playerRaceId
                }
            )
            <$> loadturn ^. Api.loadturnRst ^. Api.rstPlayers
        , _gamestateShips   =
            ( \s -> Ship
                { _shipName           = s ^. Api.shipName
                , _shipId             = s ^. Api.shipId
                , _shipOwnerId        = s ^. Api.shipOwnerId
                , _shipWarp           = s ^. Api.shipWarp
                , _shipPosition       = Position (s ^. Api.shipX) (s ^. Api.shipY)
                , _shipTargetPosition = Position (s ^. Api.shipTargetX) (s ^. Api.shipTargetY)
                , _shipResources      = Resources
                    { _resourcesMegaCredits = s ^. Api.shipMegaCredits
                    , _resourcesSupplies    = s ^. Api.shipSupplies
                    , _resourcesClans       = s ^. Api.shipClans
                    , _resourcesMinerals    = Minerals
                        { _mineralsNeutronium = s ^. Api.shipNeutronium
                        , _mineralsMolybdenum = s ^. Api.shipMolybdenum
                        , _mineralsDuranium   = s ^. Api.shipDuranium
                        , _mineralsTritanium  = s ^. Api.shipTritanium
                        }
                    }
                , _shipAmmo           = s ^. Api.shipAmmo
                , _shipHull           = mapHull (loadturn ^. Api.loadturnRst ^. Api.rstHulls) (s ^. Api.shipHullId)
                , _shipEngine         = mapEngine (loadturn ^. Api.loadturnRst ^. Api.rstEngines) (s ^. Api.shipEngineId)
                }
            )
            <$> loadturn ^. Api.loadturnRst ^. Api.rstShips
        , _gamestatePlanets =
            ( \p -> Planet
                { _planetName             = p ^. Api.planetName
                , _planetId               = p ^. Api.planetId
                , _planetOwnerId          = p ^. Api.planetOwnerId
                , _planetTemperature      = p ^. Api.planetTemp
                , _planetPosition         = Position (p ^. Api.planetX) (p ^. Api.planetY)
                , _planetMines            = p ^. Api.planetMines
                , _planetFactories        = p ^. Api.planetFactories
                , _planetDefenses          = p ^. Api.planetDefense
                , _planetResources        = Resources
                    { _resourcesMegaCredits = p ^. Api.planetMegaCredits
                    , _resourcesSupplies    = p ^. Api.planetSupplies
                    , _resourcesClans       = p ^. Api.planetClans
                    , _resourcesMinerals    = Minerals
                        { _mineralsNeutronium = p ^. Api.planetNeutronium
                        , _mineralsMolybdenum = p ^. Api.planetMolybdenum
                        , _mineralsDuranium   = p ^. Api.planetDuranium
                        , _mineralsTritanium  = p ^. Api.planetTritanium
                        }
                    }
                , _planetGroundMinerals   = Minerals
                    { _mineralsNeutronium = p ^. Api.planetGroundNeutronium
                    , _mineralsMolybdenum = p ^. Api.planetGroundMolybdenum
                    , _mineralsDuranium   = p ^. Api.planetGroundDuranium
                    , _mineralsTritanium  = p ^. Api.planetGroundTritanium
                    }
                , _planetDensityMinerals  = Minerals
                    { _mineralsNeutronium = p ^. Api.planetDensityNeutronium
                    , _mineralsMolybdenum = p ^. Api.planetDensityMolybdenum
                    , _mineralsDuranium   = p ^. Api.planetDensityDuranium
                    , _mineralsTritanium  = p ^. Api.planetDensityTritanium
                    }
                , _planetTotalMinerals    = Minerals
                    { _mineralsNeutronium = p ^. Api.planetTotalNeutronium
                    , _mineralsMolybdenum = p ^. Api.planetTotalMolybdenum
                    , _mineralsDuranium   = p ^. Api.planetTotalDuranium
                    , _mineralsTritanium  = p ^. Api.planetTotalTritanium
                    }
                , _planetCheckMinerals = Minerals
                    { _mineralsNeutronium = p ^. Api.planetCheckNeutronium
                    , _mineralsMolybdenum = p ^. Api.planetCheckMolybdenum
                    , _mineralsDuranium   = p ^. Api.planetCheckDuranium
                    , _mineralsTritanium  = p ^. Api.planetCheckTritanium
                    }
                , _planetColonistTaxRate  = p ^. Api.planetColonistTaxRate
                , _planetColonistHappiness  = p ^. Api.planetColonistHappyPoints
                , _planetColonistHappinessRate = p ^. Api.planetColHappyChange
                , _planetNativeClans      = p ^. Api.planetNativeClans
                , _planetNativeType       = toEnum $ p ^. Api.planetNativeType
                , _planetNativeGovernment = toEnum $ p ^. Api.planetNativeGovernment
                , _planetNativeTaxRate    = p ^. Api.planetNativeTaxRate
                , _planetNativeTaxValue   = p ^. Api.planetNativeTaxValue
                , _planetNativeHappiness  = p ^. Api.planetNativeHappyPoints
                , _planetNativeHappinessRate = p ^. Api.planetNativeHappyChange
                , _planetStarbase        = mapStarbase (loadturn ^. Api.loadturnRst ^. Api.rstStarbases) (p ^. Api.planetId)
                }
            )
            <$> loadturn ^. Api.loadturnRst ^. Api.rstPlanets
        , _gamestateLoadTurn = loadturn
        }
    where
        mapHull :: [Api.Hull] -> Int -> Hull
        mapHull hulls id' =
            case listToMaybe $ filter (\h -> h ^. Api.hullId == id') hulls of
                Just apiHull ->
                    Hull
                        { _hullName        = apiHull ^. Api.hullName
                        , _hullId          = apiHull ^. Api.hullId
                        , _hullCargo       = apiHull ^. Api.hullCargo
                        , _hullFuelTank    = apiHull ^. Api.hullFuelTank
                        , _hullFighterBays = apiHull ^. Api.hullFighterBays
                        , _hullLaunchers   = apiHull ^. Api.hullLaunchers
                        , _hullBeams       = apiHull ^. Api.hullBeams
                        , _hullEngines     = apiHull ^. Api.hullEngines
                        , _hullTechLevel   = apiHull ^. Api.hullTechLevel
                        }
                Nothing -> error "Hull not found!"
        mapEngine :: [Api.Engine] -> Int -> Maybe Engine
        mapEngine engines id' =
                (\e -> Engine
                        { _engineName      = e ^. Api.engineName
                        , _engineId        = e ^. Api.engineId
                        , _engineWarp      = mapEngineWarp $ e
                        , _engineTechLevel = e ^. Api.engineTechLevel
                        }
                ) <$> (listToMaybe $ filter (\e -> e ^. Api.engineId == id') engines)
        mapEngineWarp :: Api.Engine -> Map Int Int
        mapEngineWarp engine =
            Map.fromList
                [ (1, engine ^. Api.engineWarp1)
                , (2, engine ^. Api.engineWarp2)
                , (3, engine ^. Api.engineWarp3)
                , (4, engine ^. Api.engineWarp4)
                , (5, engine ^. Api.engineWarp5)
                , (6, engine ^. Api.engineWarp6)
                , (7, engine ^. Api.engineWarp7)
                , (8, engine ^. Api.engineWarp8)
                , (9, engine ^. Api.engineWarp9)
                ]
        mapStarbase :: [Api.Starbase] -> Int -> Maybe Starbase
        mapStarbase starbases planetId' =
            (\starbase -> Starbase
                { _starbaseId               = starbase ^. Api.starbaseId
                , _starbaseEngineTechLevel  = starbase ^. Api.starbaseEngineTechLevel
                , _starbaseBeamTechLevel    = starbase ^. Api.starbaseBeamTechLevel
                , _starbaseHullTechLevel    = starbase ^. Api.starbaseHullTechLevel
                , _starbaseTorpedoTechLevel = starbase ^. Api.starbaseTorpTechLevel
                , _starbaseFighters         = starbase ^. Api.starbaseFighters
                }
            ) <$> (listToMaybe $ filter (\s -> s ^. Api.starbasePlanetId == planetId') starbases)

getShipById :: Gamestate -> Int -> Maybe Ship
getShipById gamestate id' = listToMaybe $ filter (\s -> s ^. shipId == id') (gamestate ^. gamestateShips)

getPlanetById :: Gamestate -> Int -> Maybe Planet
getPlanetById gamestate id' = listToMaybe $ filter (\p -> p ^. planetId == id') (gamestate ^. gamestatePlanets)

getPlanetByName :: Gamestate -> String -> Maybe Planet
getPlanetByName gamestate name = listToMaybe $ filter (\p -> p ^. planetName == name) (gamestate ^. gamestatePlanets)

getPlanetAtShip :: Gamestate -> Ship -> Maybe Planet
getPlanetAtShip gamestate ship =
    listToMaybe $
        filter (\p -> p ^. planetPosition ^. positionX == ship ^. shipPosition ^. positionX
                   && p ^. planetPosition ^. positionY == ship ^. shipPosition ^. positionY)
               (gamestate ^. gamestatePlanets)

myPlanets :: Gamestate -> [Planet]
myPlanets gamestate =
    let myId = gamestate ^. gamestatePlayer ^. playerId
     in filter (\p -> p ^. planetOwnerId == myId) (gamestate ^. gamestatePlanets)

myPlanetsWithBase :: Gamestate -> [Planet]
myPlanetsWithBase = filter (\p -> isJust $ p ^. planetStarbase) . myPlanets

myShips :: Gamestate -> [Ship]
myShips gamestate =
    let myId = gamestate ^. gamestatePlayer ^. playerId
     in filter (\s -> s ^. shipOwnerId == myId) (gamestate ^. gamestateShips)

