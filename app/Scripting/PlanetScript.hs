module Scripting.PlanetScript where
import           Control.Monad.Free (Free, liftF)
import           Model              (Amount, Planet, Resource)

data PlanetScriptInstr next
    = GetPlanet (Planet -> next)
    | Amount Resource (Int -> next)
    | Mines (Int -> next)
    | Factories (Int -> next)
    | Defenses (Int -> next)
    | BuildMines Amount next
    | BuildFactories Amount next
    | BuildDefenses Amount next
    | SetNativeTaxRate Int next
    | SetColonistTaxRate Int next
    | BuildStarbase next
    -- | GetShips ([Ship] -> next)
    -- | GetPlanets ([Planet] -> next)
    deriving (Functor)

type PlanetScriptInstruction = Free PlanetScriptInstr

type PlanetScript = PlanetScriptInstruction ()

getPlanet :: PlanetScriptInstruction Planet
getPlanet = liftF $ GetPlanet id

amount :: Resource -> PlanetScriptInstruction Int
amount resource = liftF $ Amount resource id

mines :: PlanetScriptInstruction Int
mines = liftF $ Mines id

factories :: PlanetScriptInstruction Int
factories = liftF $ Factories id

defenses :: PlanetScriptInstruction Int
defenses = liftF $ Defenses id

buildMines :: Amount -> PlanetScriptInstruction ()
buildMines amt = liftF $ BuildMines amt ()

buildFactories :: Amount -> PlanetScriptInstruction ()
buildFactories amt = liftF $ BuildFactories amt ()

buildDefenses :: Amount -> PlanetScriptInstruction ()
buildDefenses amt = liftF $ BuildDefenses amt ()

setNativeTaxRate :: Int -> PlanetScriptInstruction ()
setNativeTaxRate amt = liftF $ SetNativeTaxRate amt ()

setColonistTaxRate :: Int -> PlanetScriptInstruction ()
setColonistTaxRate amt = liftF $ SetColonistTaxRate amt ()

buildStarbase :: PlanetScriptInstruction ()
buildStarbase = liftF $ BuildStarbase ()
