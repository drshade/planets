module Scripts where
import           Auto (Amount (..), Resource (..), Script, dropOff, flyTo,
                       pickup)

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
    [ (7, collectAndDropScript "Eeeeediot" "Van Maanan's Planet")
    , (6, collectAndDropScript "Forel" "Van Maanan's Planet")
    ]
