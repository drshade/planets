module Calcs where
import           Data.List (sortOn)
import           Model     (HasPosition, HasResources, Minerals (..),
                            NativeType (..), Planet, Race (..), Resources (..),
                            Ship, mineralsDuranium, mineralsMolybdenum,
                            mineralsNeutronium, mineralsTritanium,
                            planetColonistTaxRate, planetDensityMinerals,
                            planetFactories, planetGroundMinerals, planetMines,
                            planetNativeClans, planetNativeTaxRate,
                            planetNativeTaxValue, planetNativeType,
                            planetResources, position, resources,
                            resourcesClans, resourcesMinerals,
                            resourcesSupplies, shipResources)
import           Optics    (Lens', (^.))

production :: Planet -> Resources
production planet =
    let racialTaxModifier :: Double
        racialTaxModifier = 1 / 10

        -- https://help.planets.nu/taxes-details
        colonistTax :: Int
        colonistTax = round $
                      fromIntegral (planet ^. planetResources ^. resourcesClans)
                    * fromIntegral (planet ^. planetColonistTaxRate) / 100
                    * racialTaxModifier

        nativesTax :: Int
        nativesTax = min (planet ^. planetResources ^. resourcesClans)
                         (round $ fromIntegral (planet ^. planetNativeClans)
                            * fromIntegral (planet ^. planetNativeTaxRate) / 100
                            * fromIntegral (planet ^. planetNativeTaxValue) / 100
                            * racialTaxModifier)
                        -- Insectoids accumulate 2x tax
                        * case planet ^. planetNativeType of
                            Insectoid -> 2
                            _         -> 1

        -- https://help.planets.nu/factories - assuming that colonists are less than 10m
        supplies = planet ^. planetFactories

        mineNeu = calcMineral mineralsNeutronium
        mineMol = calcMineral mineralsMolybdenum
        mineDur = calcMineral mineralsDuranium
        mineTri = calcMineral mineralsTritanium

     in Resources
        { _resourcesMegaCredits = colonistTax + nativesTax
        , _resourcesSupplies    = supplies
        , _resourcesClans = 0
        , _resourcesMinerals = Minerals
            { _mineralsNeutronium  = mineNeu
            , _mineralsMolybdenum  = mineMol
            , _mineralsDuranium    = mineDur
            , _mineralsTritanium   = mineTri
            }
        }
    where
        calcMineral :: Lens' Minerals Int -> Int
        calcMineral mineral =
            min (planet ^. planetGroundMinerals ^. mineral)
                  $ round $
                    fromIntegral (planet ^. planetMines)
                  * (fromIntegral (planet ^. planetDensityMinerals ^. mineral) / 100 :: Double)

type TorpedoTechLevel = Int
type Lightyears = Int

-- "Mark 3c Photon" => tech 4
-- "Mark 4 Photon"  => tech 5
-- "Heavy Proton+"  => tech 6
-- "Mark 6c Photon" => tech 7
-- "Mark 7 Photon"  => tech 8
-- "Mark 8 Photon"  => tech 10
-- "Quantum Torp"   => tech 10
torpsForMinefieldSize :: Race -> TorpedoTechLevel -> Lightyears -> Int
torpsForMinefieldSize race techlevel lys =
    let minesPerTorp = techlevel ^ (2 :: Int) * (case race of Robots -> 4; _ -> 1)
        minesRequired = fromIntegral lys * (3.1416 :: Double) ** 2
     in ceiling $ minesRequired * 10 / (fromIntegral minesPerTorp)

-- Manhattan distance - not ideal
manhattanDistance :: HasPosition a => HasPosition b => a -> b -> Int
manhattanDistance p1 p2 =
    let (x1, y1) = position p1
        (x2, y2) = position p2
     in abs (x1 - x2) + abs (y1 - y2)

-- Euclidean distance
distance :: HasPosition a => HasPosition b => a -> b -> Double
distance p1 p2 =
    let (x1, y1) = position p1
        (x2, y2) = position p2
     in sqrt $ fromIntegral ((x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int))

-- Stuff within a certain range within range and return sorted by distance
withinRange :: HasPosition a => [a] -> a -> Double -> [a]
withinRange points point range =
    sortOn (\a -> distance point a)
  $ filter (\p -> let d = distance p point in d > 0 && d < range)
  $ points

totalResources :: HasResources a => [a] -> Resources
totalResources =
    foldl (\acc e -> acc <> resources e) mempty

cargoUsed :: Ship -> Int
cargoUsed ship =
    ship ^. shipResources ^. resourcesMinerals ^. mineralsMolybdenum +
    ship ^. shipResources ^. resourcesMinerals ^. mineralsDuranium +
    ship ^. shipResources ^. resourcesMinerals ^. mineralsTritanium +
    ship ^. shipResources ^. resourcesSupplies +
    ship ^. shipResources ^. resourcesClans
