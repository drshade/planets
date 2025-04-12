module Scripts where
import           Auto     (Amount (..), Resource (..), Script, dropOff, flyTo,
                           pickup)
import           Data.Map (Map, fromList)

type PlanetName = String

collectAndDropScript :: PlanetName -> PlanetName -> Script
collectAndDropScript fromPlanet toPlanet = do
    flyTo toPlanet
    dropOff Max Mc
    flyTo fromPlanet
    pickup Max Mc
    flyTo toPlanet

scripts :: [(Int, Script)]
scripts =
    [ (7, collectAndDropScript "Van Maanan's Planet" "Exodious")
    , (1, collectAndDropScript "Exodious" "Forel")
    , (6, collectAndDropScript "Forel" "Van Maanan's Planet")
    ]
