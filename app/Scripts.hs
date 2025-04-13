module Scripts where
import           Api           (hullCargo, planetName, planetOwnerId)
import           Auto          (Script, dropOff, flyTo, getPlanets, getShip,
                                getShipHull, pickup)
import           Calcs         (distance)
import           Data.Foldable (minimumBy)
import           Model         (Amount (..), Resource (..))
import           Optics        ((^.))

type PlanetName = String
type ShipId = Int

collectAndDropScript :: PlanetName -> PlanetName -> Script
collectAndDropScript fromPlanet toPlanet = do
    flyTo toPlanet
    dropOff Max Mc
    flyTo fromPlanet
    pickup Max Mc
    flyTo toPlanet

coloniseScript :: PlanetName -> Script
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

coloniseWithRatiosScript :: PlanetName -> Script
coloniseWithRatiosScript homeplanet = do
    -- Get the ship (this script is attached to)
    flyTo homeplanet

    ship <- getShip
    hull <- getShipHull ship

    pickup (Exact $ hull ^. hullCargo * 60 `div` 100) Clans
    pickup (Exact $ hull ^. hullCargo * 40 `div` 100) Supplies
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

scripts :: [(ShipId, Script)]
scripts =
    [ (1, coloniseWithRatiosScript "Aries Ceti")
    -- , (7, collectAndDropScript "Eeeeediot" "Van Maanan's Planet")
    -- , (6, collectAndDropScript "Forel" "Van Maanan's Planet")
    ]


