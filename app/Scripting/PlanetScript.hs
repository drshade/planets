module Scripting.PlanetScript where
import           Api                (PlanetUpdate, ShipUpdate)
import           Control.Monad.Free (Free, liftF)
import           Control.Monad.RWS  (RWS)
import           Data.Map           (Map)
import           Model              (Amount, Gamestate, Planet, PlanetId,
                                     Resource, ShipId)

data PlanetScriptEnvironment = PlanetScriptEnvironment Planet Gamestate
type PlanetScriptLog = [String]
data PlanetScriptState = PlanetScriptState (Map ShipId ShipUpdate) (Map PlanetId PlanetUpdate)

type PlanetScriptRWS = RWS PlanetScriptEnvironment PlanetScriptLog PlanetScriptState

data PlanetScriptInstr next
    = GetPlanet (Planet -> next)
    | Amount Resource (Int -> next)
    | Mines (Int -> next)
    | Factories (Int -> next)
    | Defenses (Int -> next)
    | BuildMines Amount next
    | BuildFactories Amount next
    | BuildDefenses Amount next
    -- | BuildStarbase next
    -- | SetNativeTaxRate Amount next
    -- | SetColonistTaxRate Amount next
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

