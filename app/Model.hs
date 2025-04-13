module Model where

data Amount = Max | Exact Int deriving (Show)
data Resource = Mc | Supplies | Clans | Neu | Dur | Tri | Mol deriving (Show)

data EntityKey = ShipEntity Int | PlanetEntity Int
    deriving (Eq, Ord, Show)
