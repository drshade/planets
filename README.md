# Planets CLI Tool
This tool is a command-line utility for the Planets.nu game that helps you track and calculate resources across your planets and ships. It connects to the Planets.nu API to fetch your current game data and provides both reporting and automation capabilities.

## Features

### Resource Tracking
- Current resources on each planet and ship
- Production capabilities of each planet
- Total resources across your empire
- Cargo usage information for your ships

### Ship Automation
The tool includes a powerful scripting system that allows you to automate ship operations. You can write scripts to:
- Move ships between planets
- Transfer resources between ships and planets
- Automate colonization missions
- Create resource collection and distribution routes

## Installation
Installing Haskell
1. Install GHCup, the Haskell toolchain installer:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

2. Follow the prompts to install GHC, Cabal, and other tools
3. Make sure to add GHCup to your PATH as instructed

Building the Project

1. Clone this repository
2. Navigate to the project directory
3. Build the project using Cabal:

```sh
cabal build
```

## Usage

1. Create a .credential file in the project root directory with your Planets.nu credentials:

```
username
password
```

2. Run the tool with a game ID:

For resource reporting:
```sh
cabal run planets -- report GAME_ID
```

For running ship automation scripts:
```sh
cabal run planets -- script GAME_ID
```

Replace GAME_ID with your Planets.nu game identifier (e.g., 643520).

## Scripting

The tool provides a domain-specific language for writing ship automation scripts. Scripts can be defined in the `app/Scripts.hs` file. Here are the available script types and their implementations:

### Resource Collection and Distribution
```haskell
collectAndDropScript :: PlanetName -> PlanetName -> ShipScript
collectAndDropScript fromPlanet toPlanet = do
    flyTo toPlanet
    dropOff Max Mc
    flyTo fromPlanet
    pickup Max Mc
    flyTo toPlanet
```
This script automates collecting resources from one planet and delivering them to another. It:
1. Flies to the destination planet
2. Drops off all available MegaCredits
3. Returns to the source planet
4. Picks up all available MegaCredits
5. Returns to the destination planet

### Colonization
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
This script automates the colonization process by:
1. Loading specific amounts of required resources:
   - 100 Clans
   - 100 Supplies
   - 400 MegaCredits
   - 100 Neutronium
2. Finding the nearest unowned planet
3. Delivering all resources to establish a colony
4. Returning to the home planet

### Colonization with Cargo Ratios
```haskell
coloniseWithRatiosScript :: PlanetName -> ShipScript
coloniseWithRatiosScript homeplanet = do
    flyTo homeplanet

    ship <- getShip

    -- Load resources based on ship's cargo capacity
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
This variant of the colonization script calculates resource amounts based on the ship's cargo capacity:
- 60% of cargo capacity for Clans
- 40% of cargo capacity for Supplies
- Fixed amounts for MC and Neutronium

### Writing Custom Scripts

You can create custom scripts using the following primitives:

#### Movement
```haskell
flyTo :: String -> ShipScriptInstruction ()
```
Moves the ship to the specified planet.

#### Resource Operations
```haskell
pickup :: Amount -> Resource -> ShipScriptInstruction ()
dropOff :: Amount -> Resource -> ShipScriptInstruction ()
```
Load or unload resources from the current planet. The amount can be:
- `Max` - Maximum available amount
- `Exact n` - Specific amount

#### Information Retrieval
```haskell
getShip :: ShipScriptInstruction Ship
getPlanets :: ShipScriptInstruction [Planet]
```
Get information about the current ship or all planets.

#### Supported Resources
```haskell
data Resource = Mc | Supplies | Clans | Neu | Dur | Tri | Mol
```
- `Mc` - MegaCredits
- `Supplies` - Supplies
- `Clans` - Clans
- `Neu` - Neutronium
- `Dur` - Duranium
- `Tri` - Tritanium
- `Mol` - Molybdenum

### Example: Custom Resource Distribution Script
Here's an example of how to write a custom script that distributes resources based on specific ratios:

```haskell
distributeResourcesScript :: PlanetName -> PlanetName -> ShipScript
distributeResourcesScript source target = do
    -- Get ship information
    ship <- getShip
    let cargoCapacity = ship ^. shipHull ^. hullCargo

    -- Load resources from source
    flyTo source
    pickup (Exact $ cargoCapacity * 30 `div` 100) Clans    -- 30% Clans
    pickup (Exact $ cargoCapacity * 40 `div` 100) Supplies -- 40% Supplies
    pickup (Exact $ cargoCapacity * 30 `div` 100) Mc       -- 30% MC

    -- Deliver to target
    flyTo target
    dropOff Max Clans
    dropOff Max Supplies
    dropOff Max Mc

    -- Return to source
    flyTo source
```

## Example Output

When running in report mode, the tool will display:

- Resources on each planet with their native race type
- Total planetary resources
- Resources on each ship with cargo and ammunition information
- Total ship resources
- Production capabilities for each planet
- Total production across your empire

# Future Development

Planned features include:

- Planet Production Potential: More detailed analysis of maximum production capabilities considering all factors
- Resource Forecasting: Estimate future resources based on:
  - Ships en route to planets
  - Expected production over multiple turns
  - Anticipated resource usage from building projects
- Strategic Recommendations: Suggestions for optimizing resource distribution
- Enhanced Scripting: Additional automation capabilities and script primitives