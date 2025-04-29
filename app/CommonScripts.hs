module CommonScripts where
import           Data.Foldable          (minimumBy)
import           Data.Function          (on)
import           Model
import           Optics                 ((^.))
import           Scripting.PlanetScript
import           Scripting.ShipScript

type PlanetName = String

collectAndDropScript :: PlanetName -> PlanetName -> ShipScript
collectAndDropScript fromPlanet toPlanet = do
    flyTo toPlanet
    dropOff Max Mc
    dropOff Max Supplies
    pickup (Exact 100) Neu
    flyTo fromPlanet
    pickup Max Mc
    pickup Max Supplies
    pickup (Exact 100) Neu
    flyTo toPlanet

coloniseScript :: PlanetName -> ShipScript
coloniseScript homeplanet = do
    flyTo homeplanet
    pickup (Exact 100) Clans
    pickup (Exact 100) Supplies
    pickup (Exact 400) Mc
    pickup (Exact 100) Neu

    -- Get the ship (this script is attached to)
    ship <- getShip

    -- Fly to nearest unowned planet
    nearestPlanet <- minimumBy (compare `on` distance ship)
                        <$> filter (\p -> p ^. planetOwnerId == 0)
                        <$> getPlanets
    flyTo (nearestPlanet ^. planetName)

    -- Drop off resources
    dropOff Max Clans
    dropOff Max Supplies
    dropOff Max Mc

    -- Fly back home
    flyTo homeplanet

    pure ()

coloniseWithRatiosScript :: PlanetName -> ShipScript
coloniseWithRatiosScript homeplanet = do
    -- Get the ship (this script is attached to)
    flyTo homeplanet

    ship <- getShip

    pickup (Exact $ ship ^. shipHull ^. hullCargo * 60 `div` 100) Clans
    pickup (Exact $ ship ^. shipHull ^. hullCargo * 40 `div` 100) Supplies
    pickup (Exact 400) Mc
    pickup (Exact 100) Neu

    -- Fly to nearest unowned planet
    nearestPlanet <- minimumBy (compare `on` distance ship)
                        <$> filter (\p -> p ^. planetOwnerId == 0)
                        <$> getPlanets
    flyTo (nearestPlanet ^. planetName)

    -- Drop off resources
    dropOff Max Clans
    dropOff Max Supplies
    dropOff Max Mc

    -- Fly back home
    flyTo homeplanet

    pure ()

buildMaxMinesScript :: PlanetScript
buildMaxMinesScript = buildMines Max

buildOneMineScript :: PlanetScript
buildOneMineScript = do
    buildMines (Exact 1)

buildOneOfEachScript :: PlanetScript
buildOneOfEachScript = do
    buildFactories (Exact 1)
    buildMines (Exact 1)
    buildDefenses (Exact 1)

