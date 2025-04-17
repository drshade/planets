module CommonScripts where
import           Data.Foldable        (minimumBy)
import           Optics               ((^.))
import           Scripting.Model
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
    nearestPlanet <- minimumBy (\p1 p2 -> compare (distance ship p1) (distance ship p2))
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
    nearestPlanet <- minimumBy (\p1 p2 -> compare (distance ship p1) (distance ship p2))
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

