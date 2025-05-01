# Planets CLI Tool

A command-line utility for Planets.nu that helps you manage resources and automate ship operations through scripting. This tool connects to the Planets.nu API to fetch your current game data and allows you to run pre-defined scripts to automate repetitive tasks.

*STILL IN ACTIVE DEVELOPMENT*

## Features

- Resource reporting across your empire
- Ship automation through customizable scripts
- Production analysis for all planets (tbd)

## Ship Automation Scripts

The tool includes a powerful scripting system that allows you to automate ship operations. Scripts are defined in `app/Scripts.hs` and attached to specific ships by their ID.

### Example Ship Scripts

#### 1. Resource Collection and Distribution

This script automates the process of collecting megacredits from one planet and delivering them to another:

```haskell
collectAndDropScript :: PlanetName -> PlanetName -> ShipScript
collectAndDropScript fromPlanet toPlanet = do
    flyTo toPlanet
    pickup Max Mc
    flyTo fromPlanet
    dropOff Max Mc
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

    -- Fly to nearest unowned planet
    nearestPlanet <- minimumBy (\p1 p2 -> compare (distance ship p1) (distance ship p2))
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

Of course you might want to base your calculations on the capacity of your ship cargo, and so you could do this like the following:

```haskell
coloniseWithRatiosScript :: PlanetName -> ShipScript
coloniseWithRatiosScript homeplanet = do
    flyTo homeplanet
    
    ship <- getShip

    pickup (Exact $ ship ^. shipHull ^. hullCargo * 60 `div` 100) Clans
    pickup (Exact $ ship ^. shipHull ^. hullCargo * 40 `div` 100) Supplies
    pickup (Exact 400) Mc
    pickup (Exact 100) Neu

    -- Fly to nearest unowned planet
    nearestPlanet <- minimumBy (\p1 p2 -> compare (distance ship p1) (distance ship p2))
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

## Assigning Scripts to Ships

To assign scripts to ships, edit the `scripts` list in `app/Scripts.hs`:

```haskell
scripts :: [(ShipId, ShipScript)]
scripts =
    [ (1, coloniseWithRatiosScript "Fred")
    , (7, collectAndDropScript "Eeeeediot" "Van Maanan's Planet")
    , (6, collectAndDropScript "Forel" "Van Maanan's Planet")
    ]
```

## How the Scripting Works

The scripting system uses Free Monads to create a domain-specific language for ship automation. This allows for writing scripts in an imperative style (even though turns run over days and weeks), while retaining no state between executions (at this stage - maybe this will change in the future?).

Each script continues from the appropriate point based on the current state of the ship - determining what step to take by examining the ship's location.

## Future Development

Planned features include:

- Planet scripts! Automate the building of mines / factories / tax rates / etc.
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
cabal run planets -- runreport -g GAME_ID
```

For running ship automation scripts:
```sh
cabal run planets -- runscript -s GAME_ID
```

Replace `GAME_ID` with your Planets.nu game identifier (e.g., 643520).