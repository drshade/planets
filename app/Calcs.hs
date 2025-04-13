module Calcs where
import           Api              (Hull, Planet, Rst, Ship, hullId, planetClans,
                                   planetColonistTaxRate, planetDensityDuranium,
                                   planetDensityMolybdenum,
                                   planetDensityNeutronium,
                                   planetDensityTritanium, planetDuranium,
                                   planetFactories, planetGroundDuranium,
                                   planetGroundMolybdenum,
                                   planetGroundNeutronium,
                                   planetGroundTritanium, planetMegaCredits,
                                   planetMines, planetMolybdenum, planetName,
                                   planetNativeClans, planetNativeTaxRate,
                                   planetNativeTaxValue, planetNativeType,
                                   planetNeutronium, planetOwnerId,
                                   planetSupplies, planetTritanium, planetX,
                                   planetY, playerId, rstHulls, rstPlanets,
                                   rstPlayer, rstShips, shipClans, shipDuranium,
                                   shipHullId, shipId, shipMegaCredits,
                                   shipMolybdenum, shipName, shipNeutronium,
                                   shipOwnerId, shipSupplies, shipTritanium,
                                   shipX, shipY)
import           Optics           (makeLenses)
import           Optics.Lens      (Lens')
import           Optics.Operators
import           Text.Printf      (printf)

class HasPosition a where
    position :: a -> (Int, Int)

instance HasPosition Planet where
    position :: Planet -> (Int, Int)
    position p = (p ^. planetX, p ^. planetY)

instance HasPosition Ship where
    position :: Ship -> (Int, Int)
    position s = (s ^. shipX, s ^. shipY)

class ResourceHolder a where
    resources :: a -> Resources

instance ResourceHolder Planet where
    resources :: Planet -> Resources
    resources p =
        Resources
            { _resourcesNeutronium  = p ^. planetNeutronium
            , _resourcesMolybdenum  = p ^. planetMolybdenum
            , _resourcesDuranium    = p ^. planetDuranium
            , _resourcesTritanium   = p ^. planetTritanium
            , _resourcesMegaCredits = p ^. planetMegaCredits
            , _resourcesSupplies    = p ^. planetSupplies
            }

instance ResourceHolder Ship where
    resources :: Ship -> Resources
    resources s =
        Resources
            { _resourcesNeutronium  = s ^. shipNeutronium
            , _resourcesMolybdenum  = s ^. shipMolybdenum
            , _resourcesDuranium    = s ^. shipDuranium
            , _resourcesTritanium   = s ^. shipTritanium
            , _resourcesMegaCredits = s ^. shipMegaCredits
            , _resourcesSupplies    = s ^. shipSupplies
            }

data Resources = Resources
    { _resourcesMegaCredits :: Int
    , _resourcesSupplies    :: Int
    , _resourcesNeutronium  :: Int
    , _resourcesMolybdenum  :: Int
    , _resourcesDuranium    :: Int
    , _resourcesTritanium   :: Int
    }

instance Show Resources where
    show :: Resources -> String
    show = showResources

showResources :: Resources -> String
showResources r = "[ " ++
        "Mc:" ++ fmt (_resourcesMegaCredits r) ++ ", " ++
        "Su:" ++ fmt (_resourcesSupplies r) ++ ", " ++
        "Ne:" ++ fmt (_resourcesNeutronium r) ++ ", " ++
        "Du:" ++ fmt (_resourcesDuranium r) ++ ", " ++
        "Tr:" ++ fmt (_resourcesTritanium r) ++ ", " ++
        "Mo:" ++ fmt (_resourcesMolybdenum r) ++
        " ]"
            where fmt = printf "%4d"

showPotentialResources :: Resources -> String
showPotentialResources r = "[ " ++
        "  /" ++ fmt (_resourcesMegaCredits r) ++ ", " ++
        "  /" ++ fmt (_resourcesSupplies r) ++ ", " ++
        "  /" ++ fmt (_resourcesNeutronium r) ++ ", " ++
        "  /" ++ fmt (_resourcesDuranium r) ++ ", " ++
        "  /" ++ fmt (_resourcesTritanium r) ++ ", " ++
        "  /" ++ fmt (_resourcesMolybdenum r) ++
        " ]"
            where fmt = printf "%4d"


instance Semigroup Resources where
  (<>) :: Resources -> Resources -> Resources
  r1 <> r2 = Resources
    { _resourcesNeutronium  = _resourcesNeutronium r1  + _resourcesNeutronium r2
    , _resourcesMolybdenum  = _resourcesMolybdenum r1  + _resourcesMolybdenum r2
    , _resourcesDuranium    = _resourcesDuranium r1    + _resourcesDuranium r2
    , _resourcesTritanium   = _resourcesTritanium r1   + _resourcesTritanium r2
    , _resourcesMegaCredits = _resourcesMegaCredits r1 + _resourcesMegaCredits r2
    , _resourcesSupplies    = _resourcesSupplies r1    + _resourcesSupplies r2
    }

instance Monoid Resources where
  mempty :: Resources
  mempty = Resources 0 0 0 0 0 0
  mappend :: Resources -> Resources -> Resources
  mappend = (<>)

makeLenses ''Resources

data Race
    = Feds
    | Lizards
    | Birds
    | Hoards
    | Privateers
    | Cyborgs
    | Crystals
    | Empire
    | Robots
    | Colonies
    | Plague

data NativeType
    = None
    | Humanoid
    | Bovinoid
    | Reptillian
    | Avian
    | Amorphous
    | Insectoid
    | Amphibian
    | Ghipsoldal
    | Siliconoid
    | Botanical
    | Unknown
    deriving (Show)

-- could probably do this better ;)
nativeType :: Int -> NativeType
nativeType 0  = None
nativeType 1  = Humanoid
nativeType 2  = Bovinoid
nativeType 3  = Reptillian
nativeType 4  = Avian
nativeType 5  = Amorphous
nativeType 6  = Insectoid
nativeType 7  = Amphibian
nativeType 8  = Ghipsoldal
nativeType 9  = Siliconoid
nativeType 10 = Botanical
nativeType _  = Unknown

data GovtType
    = Anarchy
    | PreTribal
    | EarlyTribal
    | Tribal
    | Feudal
    | Monarchy
    | Representative
    | Participatory
    | Unity

myPlanets :: Rst -> [Planet]
myPlanets rst =
    let myId = rst ^. rstPlayer ^. playerId
     in filter (\p -> p ^. planetOwnerId == myId) (rst ^. rstPlanets)

myShips :: Rst -> [Ship]
myShips rst =
    let myId = rst ^. rstPlayer ^. playerId
     in filter (\s -> s ^. shipOwnerId == myId) (rst ^. rstShips)

getHull :: Rst -> Ship -> Hull
getHull rst ship =
    let hullId' = ship ^. shipHullId
     in head $ filter (\h -> h ^. hullId == hullId') (rst ^. rstHulls)

cargoUsed :: Ship -> Int
cargoUsed ship =
    (ship ^. shipMolybdenum) +
    (ship ^. shipDuranium) +
    (ship ^. shipTritanium) +
    (ship ^. shipSupplies) +
    (ship ^. shipClans)

totalResources :: ResourceHolder a => [a] -> Resources
totalResources =
    foldl (\acc e -> acc <> resources e) mempty

potentialProduction :: Planet -> Resources
potentialProduction planet =
    let racialTaxModifier :: Double
        racialTaxModifier = 1 / 10

        nativesTax = (round $ fromIntegral (planet ^. planetNativeClans)
                            * fromIntegral (planet ^. planetNativeTaxRate) / 100
                            * fromIntegral (planet ^. planetNativeTaxValue) / 100
                            * racialTaxModifier)
                        -- Insectoids accumulate 2x tax
                        * case nativeType $ planet ^. planetNativeType of
                            Insectoid -> 2
                            _         -> 1

     in Resources
        { _resourcesMegaCredits = nativesTax
        , _resourcesSupplies    = 0
        , _resourcesNeutronium  = 0
        , _resourcesMolybdenum  = 0
        , _resourcesDuranium    = 0
        , _resourcesTritanium   = 0
        }

production :: Planet -> Resources
production planet =
    let racialTaxModifier :: Double
        racialTaxModifier = 1 / 10

        -- https://help.planets.nu/taxes-details
        colonistTax :: Int
        colonistTax = round $
                      fromIntegral (planet ^. planetClans)
                    * fromIntegral (planet ^. planetColonistTaxRate) / 100
                    * racialTaxModifier

        nativesTax :: Int
        nativesTax = min (planet ^. planetClans)
                         (round $ fromIntegral (planet ^. planetNativeClans)
                            * fromIntegral (planet ^. planetNativeTaxRate) / 100
                            * fromIntegral (planet ^. planetNativeTaxValue) / 100
                            * racialTaxModifier)
                        -- Insectoids accumulate 2x tax
                        * case nativeType $ planet ^. planetNativeType of
                            Insectoid -> 2
                            _         -> 1

        -- https://help.planets.nu/factories - assuming that colonists are less than 10m
        supplies = planet ^. planetFactories

        mineNeu = calcMineral planetDensityNeutronium planetGroundNeutronium
        mineMol = calcMineral planetDensityMolybdenum planetGroundMolybdenum
        mineDur = calcMineral planetDensityDuranium planetGroundDuranium
        mineTri = calcMineral planetDensityTritanium planetGroundTritanium

     in Resources
        { _resourcesMegaCredits = colonistTax + nativesTax
        , _resourcesSupplies    = supplies
        , _resourcesNeutronium  = mineNeu
        , _resourcesMolybdenum  = mineMol
        , _resourcesDuranium    = mineDur
        , _resourcesTritanium   = mineTri
        }
    where
        calcMineral :: Lens' Planet Int -> Lens' Planet Int -> Int
        calcMineral density ground =
            min (planet ^. ground)
                  $ round $
                    fromIntegral (planet ^. planetMines)
                  * (fromIntegral (planet ^. density) / 100 :: Double)

type TorpedoTechLevel = Int
type Lightyears = Int

torpsForMinefieldSize :: Race -> TorpedoTechLevel -> Lightyears -> Int
torpsForMinefieldSize race techlevel lys =
    let minesPerTorp = techlevel ^ (2 :: Int) * (case race of Robots -> 4; _ -> 1)
        minesRequired = fromIntegral lys * (3.1416 :: Double) ** 2
     in ceiling $ minesRequired * 10 / (fromIntegral minesPerTorp)

getShipByName :: Rst -> String -> [Ship]
getShipByName rst name =
    filter (\s -> s ^. shipName == name) (rst ^. rstShips)

getShipById :: Rst -> Int -> Maybe Ship
getShipById rst id' =
    case filter (\s -> s ^. shipId == id') (rst ^. rstShips) of
        []    -> Nothing
        (s:_) -> Just s -- can really only be one

getPlanetAtShip :: Rst -> Ship -> Maybe Planet
getPlanetAtShip rst ship =
    case planetsAtPosition of
        []    -> Nothing
        (p:_) -> Just p -- can really only be one
    where
        planetsAtPosition :: [Planet]
        planetsAtPosition =
            filter (\p -> p ^. planetX == ship ^. shipX && p ^. planetY == ship ^. shipY)
                   (rst ^. rstPlanets)

getPlanetByName :: Rst -> String -> Maybe Planet
getPlanetByName rst name =
    case filter (\p -> p ^. planetName == name) (rst ^. rstPlanets) of
        []    -> Nothing
        (p:_) -> Just p -- can really only be one

distance :: HasPosition a => HasPosition b => a -> b -> Int
distance p1 p2 =
    let (x1, y1) = position p1
        (x2, y2) = position p2
     in abs (x1 - x2) + abs (y1 - y2)
