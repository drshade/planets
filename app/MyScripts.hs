module MyScripts where
import           CommonScripts
import           Model
import           Scripting.PlanetScript (PlanetScript)
import           Scripting.ShipScript

type ShipScriptAssignment = (ShipId, ShipScript)
type PlanetScriptAssignment = (PlanetId, PlanetScript)

data GameDef = GameDef Int [ShipScriptAssignment] [PlanetScriptAssignment]

game :: GameId -> GameDef
game id' = GameDef id' [] []

(^->) :: GameDef -> ShipScriptAssignment -> GameDef
GameDef id' ships planets ^-> assignment = GameDef id' (assignment : ships) planets

(@->) :: GameDef -> PlanetScriptAssignment -> GameDef
GameDef id' ships planets @-> assignment = GameDef id' ships (assignment : planets)

(==>) :: id -> s -> (id, s)
(==>) id' script' = (id', script')

-- to glue scripts together
(>>=>) :: Monad m => m () -> m () -> m ()
(>>=>) a b = a >>= const b

-- TBD: Figure out the precedence to write this:
-- @-> (2 ==> buildMaxMinesScript >>=> setTaxRate)
-- infixl 5 ==>
-- infixl 6 @->
-- infixl 5 >>=>

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

    -- Lets try this thing
    -- game 643598

    -- Basic training game
    let _homeplanet = "Hiperborealia"
     in

    game 644461
        ^-> (1 ==> patrolScript ["Hiperborealia", "Phorax", "Rsky Business"])
        -- ^-> (1 ==> coloniseScript homeplanet)
        -- ^-> (3 ==> collectAndDropScript "Kapteyn's Planet" homeplanet)
        -- ^-> (4 ==> collectAndDropScript "Serada 9" homeplanet)
        -- @-> (5 ==> buildOneOfEachScript)
        @-> (2 ==> (buildMaxMinesScript >>=> setTaxRate))

    :[]
