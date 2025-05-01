module Scripting.Types where
import           Api               (PlanetUpdate, ShipUpdate)
import           Control.Monad.RWS (RWS)
import           Data.Map          (Map)
import           Model             (Gamestate, PlanetId, ShipId)

-- Setup the types for a full RWS monad
data ScriptEnvironment a = ScriptEnvironment Gamestate a
type ScriptLog = [String]
data ScriptState = ScriptState (Map ShipId ShipUpdate) (Map PlanetId PlanetUpdate)
type ScriptRWS a = RWS (ScriptEnvironment a) ScriptLog ScriptState
