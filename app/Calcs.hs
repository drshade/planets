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
                                   planetMines, planetMolybdenum,
                                   planetNativeClans, planetNativeTaxRate,
                                   planetNativeTaxValue, planetNativeType,
                                   planetNeutronium, planetOwnerId,
                                   planetSupplies, planetTritanium, playerId,
                                   rstHulls, rstPlanets, rstPlayer, rstShips,
                                   shipClans, shipDuranium, shipHullId,
                                   shipMegaCredits, shipMolybdenum,
                                   shipNeutronium, shipOwnerId, shipSupplies,
                                   shipTritanium)
import           Optics           (makeLenses)
import           Optics.Lens      (Lens')
import           Optics.Operators
import           Text.Printf      (printf)

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
    show r = "[ " ++
        "MCr: " ++ fmt (_resourcesMegaCredits r) ++ ", " ++
        "Sup: " ++ fmt (_resourcesSupplies r) ++ ", " ++
        "Neu: " ++ fmt (_resourcesNeutronium r) ++ ", " ++
        "Mol: " ++ fmt (_resourcesMolybdenum r) ++ ", " ++
        "Dur: " ++ fmt (_resourcesDuranium r) ++ ", " ++
        "Tri: " ++ fmt (_resourcesTritanium r) ++
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
