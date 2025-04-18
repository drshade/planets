module MyScripts where
import           CommonScripts
import           Scripting.Model
import           Scripting.ShipScript

-- To move to PlanetsScripting one day
type PlanetScript = ()

(==>) :: ShipId -> ShipScript -> ShipScriptAssignment
(==>) id' script' = (id', script')

type ShipScriptAssignment = (ShipId, ShipScript)
type PlanetScriptAssignment = (PlanetId, PlanetScript)

data GameDef = GameDef Int [ShipScriptAssignment] [PlanetScriptAssignment]

game :: GameId -> GameDef
game id' = GameDef id' [] []

(^->) :: GameDef -> ShipScriptAssignment -> GameDef
GameDef id' ships planets ^-> assignment = GameDef id' (assignment : ships) planets

(@->) :: GameDef -> PlanetScriptAssignment -> GameDef
GameDef id' ships planets @-> assignment = GameDef id' ships (assignment : planets)

scripts :: [GameDef]
scripts =
    -- Westville Game
    -- game 641474
    --     ^-> (1 ==> coloniseWithRatiosScript "Fred")
    --     ^-> (2 ==> coloniseWithRatiosScript "Forel")
    -- :

    -- Sector 7777 Game
    -- game 643510
    --     ^-> (1 ==> coloniseWithRatiosScript "Frank")
    -- :

    -- Basic training game
    let homeplanet = "Hiperborealia"
     in

    game 644461
        ^-> (1 ==> coloniseScript homeplanet)
        -- ^-> (3 ==> collectAndDropScript "Kapteyn's Planet" "Fred")
        -- ^-> (4 ==> collectAndDropScript "Serada 9" "Fred")

    :[]
