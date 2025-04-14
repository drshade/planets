module Scripting.ShipScript where
import           Api                (PlanetUpdate, ShipUpdate)
import           Control.Monad.Free (Free, liftF)
import           Control.Monad.RWS  (RWS)
import           Data.Map           (Map)
import           Scripting.Model

data ShipScriptEnvironment = ShipScriptEnvironment Ship Gamestate
type ShipScriptLog = [String]
data ShipScriptState = ShipScriptState (Map ShipId ShipUpdate) (Map PlanetId PlanetUpdate)

type ShipScriptRWS = RWS ShipScriptEnvironment ShipScriptLog ShipScriptState

data ShipScriptInstr next
    = FlyTo String next
    | Pickup Amount Resource next
    | DropOff Amount Resource next
    | GetShip (Ship -> next)
    | GetPlanets ([Planet] -> next)
    deriving (Functor)

type ShipScriptInstruction = Free ShipScriptInstr

type ShipScript = ShipScriptInstruction ()

flyTo :: String -> ShipScriptInstruction ()
flyTo planet = liftF $ FlyTo planet ()

pickup :: Amount -> Resource -> ShipScriptInstruction ()
pickup amt resource = liftF $ Pickup amt resource ()

dropOff :: Amount -> Resource -> ShipScriptInstruction ()
dropOff amt resource = liftF $ DropOff amt resource ()

getShip :: ShipScriptInstruction Ship
getShip = liftF $ GetShip id

getPlanets :: ShipScriptInstruction [Planet]
getPlanets = liftF $ GetPlanets id
