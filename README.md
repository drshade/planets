# Planets CLI Tool

A command-line utility for Planets.nu that helps you manage resources and automate ship operations through scripting. This tool connects to the Planets.nu API to fetch your current game data and allows you to run pre-defined scripts to automate repetitive tasks.

*STILL IN ACTIVE DEVELOPMENT*

## Features

- Resource reporting across your empire
- Ship automation through customizable scripts
- Production analysis for all planets (tbd)

## Ship Automation Scripts

The tool includes a powerful scripting system that allows you to automate ship operations. Scripts are defined in `app/MyScripts.hs` and attached to specific ships by their ID.

### Example Ship Scripts

#### 1. Resource Collection and Distribution

This script automates the process of collecting megacredits from one planet and delivering them to another:

```haskell
collectAndDropScript :: PlanetName -> PlanetName -> ShipScript
collectAndDropScript fromPlanet toPlanet = do
    flyTo toPlanet
    dropOff Max Mc
    flyTo fromPlanet
    pickUp Max Mc
    flyTo toPlanet
```

#### 2. Basic Colonization

Automates the colonization of new planets:

```haskell
coloniseScript :: PlanetName -> ShipScript
coloniseScript homeplanet = do
    flyTo homeplanet
    pickup (Exact 100) Clans
    pickup (Exact 100) Supplies
    pickup (Exact 400) Mc
    pickup (Exact 100) Neu

    -- Get the ship (this script is attached to)
    ship <- getShip

    -- Find and fly to nearest unowned planet
    nearestPlanet <- minimumBy (compare `on` distance ship)
                        <$> filter (\p -> p ^. planetOwnerId == 0)
                        <$> getPlanets
    flyTo (nearestPlanet ^. planetName)

    -- Drop off resources
    dropOff Max Clans
    dropOff Max Supplies
    dropOff Max Mc

    -- Fly back home
    flyTo homeplanet
```

#### 3. Adaptive Colonization

Of course you might want to base your calculations on the capacity of your ship cargo, and so you could do these calcs quite easily too by inspecting your own ship's hull capacity:

```haskell
    ship <- getShip

    -- Determine your total cargo capacity
    let totalCargoCapacity = ship ^. shipHull ^. hullCargo

    -- Pickup upto 60% of that capacity in Clans
    pickup (Exact $ totalCargoCapacity * 60 `div` 100) Clans
```

If you want to get even fancier - why not calculate how much capacity you have free rather that total?

```haskell
    ship <- getShip

    -- Determine your total cargo capacity
    let freeCapacity = ship ^. shipHull ^. hullCargo - cargoUsed ship

    -- Pickup upto 100% of your free capacity in Clans
    pickup (Exact $ freeCapacity) Clans
```

## Planet automation scripts

You can build and assign Planet Scripts too! For example:

```haskell
-- A script to build a single mine, factory and defense every turn
buildOneOfEachScript :: PlanetScript
buildOneOfEachScript = do
    buildFactories (Exact 1)
    buildMines (Exact 1)
    buildDefenses (Exact 1)
```

Or set native tax rate to 5%:

```haskell
setTaxRate :: PlanetScript
setTaxRate = setNativeTaxRate 5
```

## Assigning Scripts to Ships and Planets

To assign scripts to ships in `app/MyScripts.hs`:

```haskell
scripts :: [GameDef]
scripts =
    let homeplanet = "Hiperborealia"
     in
    game 644461 -- <- specify the game id!
        -- Ship id 1 assigned the patrolScript with various parameters
        ^-> (1 ==> patrolScript [homeplanet, "Phorax", "Rsky Business"])
        -- Ship id 2 assigned the coloniseScript with various parameters
        ^-> (2 ==> coloniseScript homeplanet)
        -- etc.
        ^-> (3 ==> collectAndDropScript "Kapteyn's Planet" homeplanet)
        ^-> (4 ==> collectAndDropScript "Serada 9" homeplanet)
        
        -- Planet id 5 assigned the buildOneOfEachScript
        @-> (5 ==> buildOneOfEachScript)
        -- and so on
        @-> (6 ==> setTaxRate)

    :[]
```

## Composition

Of course all planet and ship scripts are composable, so you can combine and mix 'em up however you like:

```haskell
buildOneOfEachScript >>=> setTaxRate
```

## How the Scripting Works

The scripting system uses Free Monads to create a domain-specific language for ship automation. This allows for writing scripts in an imperative style (even though turns run over days and weeks), while retaining no state between executions (at this stage - maybe this will change in the future?).

Each script continues from the appropriate point based on the current state of the ship - determining what step to take by examining the ship's location.

## Future Development

Planned features include:

- Planet Production Potential: More detailed analysis of maximum production capabilities
- Resource Forecasting: Estimate future resources based on ships en route and production
- Strategic Recommendations: Optimizing resource distribution
- Enhanced Scripting: Additional automation capabilities for fleet coordination

## Installation

### Installing Haskell

I recommend using `GHCup` and follow the steps to install a working GHC environment.

### Building the Project

Once haskell & cabal are installed, you should be able to build the project using:

```sh
cabal build
```

## Basic Usage

Create a `.credential` file in the project root directory with your Planets.nu credentials:

```
username
password
```

2. Run the tool with a game ID:

For resource reporting:
```sh
cabal run planets -- runproductionreport -g GAME_ID
```

For running ship automation scripts:
```sh
cabal run planets -- runscript
```

Replace `GAME_ID` with your Planets.nu game identifier (e.g., 643520).