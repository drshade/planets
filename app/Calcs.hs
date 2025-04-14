module Calcs where
import           Optics          (Lens', (^.))
import           Scripting.Model (Minerals (..), NativeType (..), Planet,
                                  Race (..), Resources (..), mineralsDuranium,
                                  mineralsMolybdenum, mineralsNeutronium,
                                  mineralsTritanium, planetColonistTaxRate,
                                  planetDensityMinerals, planetFactories,
                                  planetGroundMinerals, planetMines,
                                  planetNativeClans, planetNativeTaxRate,
                                  planetNativeTaxValue, planetNativeType,
                                  planetResources, resourcesClans,
                                  resourcesMinerals)

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

torpsForMinefieldSize :: Race -> TorpedoTechLevel -> Lightyears -> Int
torpsForMinefieldSize race techlevel lys =
    let minesPerTorp = techlevel ^ (2 :: Int) * (case race of Robots -> 4; _ -> 1)
        minesRequired = fromIntegral lys * (3.1416 :: Double) ** 2
     in ceiling $ minesRequired * 10 / (fromIntegral minesPerTorp)

