module MyScripts where
import           CommonScripts
import           Model
import           Scripting.PlanetScript (PlanetScript)
import           Scripting.ShipScript

data ScriptAssignment
    = ShipScriptAssignment ShipId ShipScript
    | PlanetScriptAssignment PlanetId PlanetScript

data GameDef = GameDef Int [ScriptAssignment]

game :: GameId -> [ScriptAssignment] -> GameDef
game id' assignments = GameDef id' assignments

ship :: ShipId -> ShipScript -> ScriptAssignment
ship id' script' = ShipScriptAssignment id' script'

plnt :: PlanetId -> PlanetScript -> ScriptAssignment
plnt id' script' = PlanetScriptAssignment id' script'

scripts :: [GameDef]
scripts =
        [ let _homeplanet = "Hiperborealia"
           in game 644461
                [ plnt 2 buildMaxMinesScript
                , ship 1 $ patrolScript ["Hiperborealia", "Phorax", "Rsky Business"]
                ]
        ]

