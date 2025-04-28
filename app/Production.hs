module Production where

import           Model                 (Gamestate, Minerals,
                                        NativeType (Amorphous, Bovinoid, Insectoid),
                                        Planet, Race (..), distance,
                                        gamestatePlayer, mineralsDuranium,
                                        mineralsMolybdenum, mineralsNeutronium,
                                        mineralsTritanium, myPlanets,
                                        myPlanetsWithBase,
                                        planetColonistHappiness,
                                        planetColonistHappinessRate,
                                        planetColonistTaxRate,
                                        planetDensityMinerals, planetFactories,
                                        planetGroundMinerals, planetId,
                                        planetMines, planetName,
                                        planetNativeClans,
                                        planetNativeHappinessRate,
                                        planetNativeTaxRate,
                                        planetNativeTaxValue, planetNativeType,
                                        planetResources, planetTemperature,
                                        playerRace, resourcesClans,
                                        resourcesMegaCredits, resourcesMinerals,
                                        resourcesSupplies, withinRange)
import           Optics                ((^.))
import           Optics.Lens           (Lens')
import           System.Console.Pretty (Color (..), color)
import           Text.Printf           (printf)

percent :: Int -> Double
percent = (\x -> x / 100) . fromIntegral

colonistTaxes :: Race -> Int -> Int -> Int -> Int
colonistTaxes race colonistClans colonistHappiness colonistTaxRate  =
    let raceTaxModifier = case race of Feds -> 2.0 :: Double; _ -> 1.0
     in if colonistHappiness > 30 then
            min 5000    -- no more than 5000 allowed
            $ round     -- seems to be rounded not floored
            $ fromIntegral colonistClans * percent colonistTaxRate * raceTaxModifier
            / 10.0
        else 0          -- Zero tax when happiness 30 and below

nativesTax :: NativeType -> Int -> Int -> Int -> Int -> Int
nativesTax nativeType colonistClans nativeClans nativeTaxValue nativeTaxRate =
    min 5000
                -- Clans collect 2x tax from Insectoids, 0x from Amorphous
        $ min   (colonistClans * case nativeType of Insectoid -> 2; Amorphous -> 0; _ -> 1)
                (round $ fromIntegral nativeClans
                       * percent nativeTaxRate
                       * percent nativeTaxValue
                       / 10)

colonistMaximumPopulation :: Race -> Int -> Int
colonistMaximumPopulation race temperature
    -- Crystals
    | race == Crystals = temperature * 1000
    -- HOT Planets & Robots, Hoards or Colonies
    | temperature > 84 && race == Robots || race == Hoards || race == Colonies =
        max 60 $ floor $ ((20099.9 :: Double) - (200 * fromIntegral temperature)) / 10
    -- COLD Planet & Rebels
    | temperature < 20 && race == Rebels =
        max 90000 $ floor $ ((299.9 :: Double) + (200 * fromIntegral temperature)) / 10
    -- Everyone else
    | temperature > 84  = floor $ ((20099.9 :: Double) - (200 * fromIntegral temperature)) / 10
    | temperature < 15  = floor $ ((299.9 :: Double) + (200 * fromIntegral temperature)) / 10
    | otherwise         = round $ sin (3.14 * (100 - fromIntegral temperature) / 100 :: Double) * 100000

maximumMines :: Int -> Int
maximumMines colonistClans
    | colonistClans <= 200   = colonistClans
    | otherwise              = 200 + (floor $ sqrt (fromIntegral colonistClans - 200 :: Double))

maximumFactories :: Int -> Int
maximumFactories colonistClans
    | colonistClans <= 100   = colonistClans
    | otherwise              = 100 + (floor $ sqrt (fromIntegral colonistClans - 100 :: Double))

-- Planets ordered by closeness to homeworld
productionReport :: Gamestate -> IO ()
productionReport gamestate = do
    mapM_ base myBasePlanets
    where
        myBasePlanets = myPlanetsWithBase gamestate
        myRace = gamestate ^. gamestatePlayer ^. playerRace

        base :: Planet -> IO ()
        base basePlanet = do
            putStrLn $ "------------------------------------------"
            putStrLn $ "🛰️ Starbase " ++ (basePlanet ^. planetName) ++ " ->"
            putStrLn $ "------------------------------------------"
            planet basePlanet basePlanet
            let closestPlanets = withinRange (myPlanets gamestate) basePlanet 162
            mapM_ (planet basePlanet) closestPlanets
            putStrLn $ "------------------------------------------"
            putStrLn ""

        planet :: Planet -> Planet -> IO ()
        planet basePlanet planet' = do
            -- Potential taxes if we set happiness growth to 0% & maximum population
            let currentColonistPopulation = planet' ^. planetResources ^. resourcesClans
            let maxColonistPopulation = colonistMaximumPopulation myRace (planet' ^. planetTemperature)

            putStrLn
                $ "🌍 " ++ printf "%3d" (planet' ^. planetId)
                ++ ": " ++ planet' ^. planetName ++ " - "
                ++ show (planet' ^. planetNativeType)
                ++ " ["
                ++ printf "%3d" (planet' ^. planetMines)
                ++ "/"
                ++ printf "%3d" (maximumMines currentColonistPopulation)
                ++ "(↑"
                ++ printf "%3d" (maximumMines maxColonistPopulation)
                ++ ") mines, "
                ++ printf "%3d" (planet' ^. planetFactories)
                ++ "/"
                ++ printf "%3d" (maximumFactories currentColonistPopulation)
                ++ " ("
                ++ printf "%3d" (maximumFactories maxColonistPopulation)
                ++ "↑) factories"
                ++ "] - "
                ++ printf "%3.1f" (distance basePlanet planet')
                ++ "ly away"

            -- TAXES
            let currentColonistTax = colonistTaxes myRace (planet' ^. planetResources ^. resourcesClans) (planet' ^. planetColonistHappiness) (planet' ^. planetColonistTaxRate)
            let currentNativesTax = nativesTax (planet' ^. planetNativeType) (planet' ^. planetResources ^. resourcesClans) (planet' ^. planetNativeClans) (planet' ^. planetNativeTaxValue) (planet' ^. planetNativeTaxRate)

            let potentialColonistTax = colonistTaxes myRace (planet' ^. planetResources ^. resourcesClans) (planet' ^. planetColonistHappiness) (planet' ^. planetColonistTaxRate + planet' ^. planetColonistHappinessRate)
            let _potentialColonistTaxWithGrowth = colonistTaxes myRace maxColonistPopulation (planet' ^. planetColonistHappiness) (planet' ^. planetColonistTaxRate + planet' ^. planetColonistHappinessRate)

            let potentialNativesTax = nativesTax (planet' ^. planetNativeType) (maxColonistPopulation) (planet' ^. planetNativeClans) (planet' ^. planetNativeTaxValue) (planet' ^. planetNativeTaxRate + planet' ^. planetNativeHappinessRate)

            -- MegaCredits
            putStrLn $ "MegaCredits : "
                    ++ printf "%4d" (planet' ^. planetResources ^. resourcesMegaCredits)
                    ++ " -> "
                    ++ printf "%4d" (currentColonistTax + currentNativesTax)
                    ++ " next"
                    ++ " (" ++ printf "%4d" currentColonistTax
                    ++ (if currentColonistTax /= potentialColonistTax
                            then color Yellow $ printf "%4d↑" potentialColonistTax
                            else "")
                    ++ " from colonists + "
                    ++ printf "%4d" currentNativesTax
                    ++ (if currentNativesTax /= potentialNativesTax
                            then color Yellow $ printf "%4d↑" potentialNativesTax
                            else "")
                    ++ " from natives)"

            -- Supplies
            putStrLn $ "Supplies    : "
                    ++ printf "%4d" (planet' ^. planetResources ^. resourcesSupplies)
                    ++ " -> "
                    ++ case (planet' ^. planetNativeType) of
                            Bovinoid ->
                                let extras :: Int
                                    extras = min (currentColonistPopulation) (round $ (fromIntegral (planet' ^. planetNativeClans) / 100 :: Double))
                                 in color Green $ printf "%4d" (planet' ^. planetFactories + extras) ++ " next - Bovinoid bonus = " ++ printf "%4d" extras
                            _ -> printf "%4d" (planet' ^. planetFactories) ++ " next"


            -- MINERALS & MINING

            putStrLn $ minerals (planet' ^. planetMines) (planet' ^. planetResources ^. resourcesClans) maxColonistPopulation (planet' ^. planetResources ^. resourcesMinerals) (planet' ^. planetGroundMinerals) (planet' ^. planetDensityMinerals)

            putStrLn ""

        minerals :: Int -> Int -> Int -> Minerals -> Minerals -> Minerals -> String
        minerals mines currentColonistClans maxColonistClans available ground density =
                item "Ne" mineralsNeutronium ++ "\n" ++
                item "Du" mineralsDuranium ++ "\n" ++
                item "Tr" mineralsTritanium ++ "\n" ++
                item "Mo" mineralsMolybdenum
            where
                productionNextTurn lens' = min (ground ^. lens') (mines * (density ^. lens') `div` 100)
                _productionNextTurns turns lens' = min (ground ^. lens') (productionNextTurn lens' * turns)
                productionMaximum numclans lens' = min (ground ^. lens') (maximumMines numclans * (density ^. lens') `div` 100)

                item :: String -> Lens' Minerals Int -> String
                item mnemonic lens' =
                    mnemonic ++ ": "
                        ++ quant pad (available ^. lens') qavailable
                        ++ " / "
                        ++ quant pad (ground ^. lens') qground
                        ++ " @ "
                        ++ quant padpercent (density ^. lens') qdensity
                        ++ "%"
                        ++ " -> " ++ quant pad (productionNextTurn lens') qavailable ++ " next ("
                        -- ++ pad (productionNextTurns 5 lens') ++ ","
                        -- ++ pad (productionNextTurns 10 lens') ++ ","
                        -- ++ pad (productionNextTurns 20 lens') ++ " over 5, 10, 20 turns"
                        ++ quant pad (productionMaximum currentColonistClans lens') qavailable ++ " max, "
                        ++ quant pad (productionMaximum maxColonistClans lens') qavailable ++ "↑)"
                pad = "%5d"
                padpercent = "%3d"
                qavailable = 500
                qground = 2500
                qdensity = 50

                quant :: String -> Int -> Int -> String
                quant fmt val threshold =
                    if val > threshold
                        then color Green $ printf fmt val
                        else color Default $ printf fmt val

