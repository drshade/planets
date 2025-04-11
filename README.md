# Planets CLI Tool
This tool is a command-line utility for the Planets.nu game that helps you track and calculate resources across your planets and ships. It connects to the Planets.nu API to fetch your current game data and displays:

- Current resources on each planet and ship
- Production capabilities of each planet
- Total resources across your empire
- Cargo usage information for your ships

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

```
cabal run planets -- GAME_ID
```

Replace GAME_ID with your Planets.nu game identifier (e.g., 643520).

## Example Output

The tool will display:

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