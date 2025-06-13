# Automating a game from 1992 - Using Free Monads!

## The Problem: VGAP and Turn-Based Games

I recently got invited by some old school friends to join a VGAPlanets game; a game which I last played around 1993 or 1994 when I was a second-former at high school. This involved careful management and muling of stiffy disks to and from school each day, delivering them to a particular Bryan Johnston, who would then dutifully copy each players "turn file" to his host machine, run the host, then copy the next day's turns back to each stiffy disk before bringing them back to school to us the next day. Supremely fun, but not without it's challenges (corrupted disks, someone sick, losing their stiffy, copying the wrong files - it was a nightmare). Nowadays thankfully someone called Joshua has re-written large chunks of this game and now you can play it online with your friends very easily. Of course - you still have to wait a day, or wait for everyone to be ready and the game is basically the same as it ever way - highly detailed and full of numbers, and runs at an absolute snails pace. An absolute glacier of a game.

So it's very fun - but man does it become a nightmare once you have double digits ships and planets, and unless you take copious notes and write down your strategies you will easily approach each turn as a goldfish does each day. 




When I started working on my Planets.nu automation tool, I faced a common challenge in turn-based strategy games: the Very Good Automation Problem (VGAP). In games like Planets.nu, turns happen once every few days, and players need to make decisions that will play out over multiple turns. This creates a unique challenge for automation tools.

Traditional automation approaches often rely on maintaining state between turns, tracking what ships were doing, and resuming those actions. But this approach has several drawbacks:

1. **State Management Complexity**: You need to store and retrieve the state of each ship's mission between turns.
2. **Fragility**: If the game state changes unexpectedly (e.g., a ship is destroyed), your stored state becomes invalid.
3. **Limited Adaptability**: Pre-defined state machines can't easily adapt to changing game conditions.

## The Solution: Stateless Ship Behavior Scripts

I wanted a different approach: scripts that could run once per turn, figure out where they are in the game world, and continue their mission without needing to remember what they were doing previously. This is similar to how a human player might approach the game - they look at the current state and decide what to do next.

For example, a colonization script should:
1. Check if the ship is at the home planet
2. If yes, load resources and fly to the nearest unowned planet
3. If at the target planet, drop off resources and return home
4. If somewhere else, fly to the home planet

The script doesn't need to remember which step it was on - it can figure that out by examining the current game state.

## Enter Free Monads

To implement this approach, I turned to Free Monads, a functional programming pattern that allows you to define a domain-specific language (DSL) and interpret it in different ways. Here's how I applied it to my ship automation problem:

### 1. Defining the Ship Script DSL

```haskell
data ShipScriptInstruction a
  = Pure a
  | FlyTo String (ShipScriptInstruction a)
  | Pickup Amount Resource (ShipScriptInstruction a)
  | DropOff Amount Resource (ShipScriptInstruction a)
  | GetShip (Ship -> ShipScriptInstruction a)
  | GetPlanets ([Planet] -> ShipScriptInstruction a)
  deriving (Functor)

type ShipScript = Free ShipScriptInstruction ()
```

This DSL defines the basic operations a ship can perform: flying to planets, picking up and dropping off resources, and getting information about the ship and planets.

### 2. Creating Helper Functions for the DSL

```haskell
flyTo :: String -> ShipScript
flyTo planet = liftF $ FlyTo planet (Pure ())

pickup :: Amount -> Resource -> ShipScript
pickup amount resource = liftF $ Pickup amount resource (Pure ())

dropOff :: Amount -> Resource -> ShipScript
dropOff amount resource = liftF $ DropOff amount resource (Pure ())

getShip :: ShipScript Ship
getShip = liftF $ GetShip Pure

getPlanets :: ShipScript [Planet]
getPlanets = liftF $ GetPlanets Pure
```

These helper functions make it easy to write scripts using the DSL.

### 3. Implementing the Interpreter

The interpreter takes a script and the current game state, and produces a list of updates to apply to the game:

```haskell
restoreAndRun :: ShipScriptEnvironment -> ShipScript -> (ShipScriptLog, ShipScriptState)
restoreAndRun env script = runRWS (interpretGS script) env (ShipScriptState empty empty)
```

The interpreter handles each instruction based on the current game state, without needing to know what happened in previous turns.

## Example: A Colonization Script

Here's an example of a colonization script that uses this approach:

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

This script doesn't need to remember which step it was on. Each turn, it:
1. Tries to fly to the home planet (if already there, this is a no-op)
2. Picks up resources (if already loaded, this is a no-op)
3. Finds the nearest unowned planet
4. Flies to that planet
5. Drops off resources
6. Returns home

## Benefits of This Approach

1. **Simplicity**: Scripts are easy to write and understand, focusing on what should happen rather than how to track state.
2. **Robustness**: If a ship is destroyed or the game state changes unexpectedly, the script will adapt on the next turn.
3. **Composability**: Scripts can be combined and reused in different ways.
4. **Testability**: The Free Monad approach makes it easy to test scripts without needing the full game environment.

## Challenges and Lessons Learned

While this approach worked well for my use case, there were some challenges:

1. **Performance**: Free Monads can have some performance overhead compared to more direct approaches.
2. **Learning Curve**: The Free Monad pattern can be difficult to understand for developers not familiar with functional programming.
3. **Debugging**: Debugging Free Monad-based code can be challenging, as the execution flow is less straightforward.

Despite these challenges, the benefits of the Free Monad approach for this specific use case outweighed the drawbacks. The ability to write stateless scripts that adapt to the current game state made the automation tool much more robust and flexible.

## Conclusion

Using Free Monads for game automation in turn-based strategy games like Planets.nu offers a powerful way to create stateless, adaptive scripts. By focusing on what should happen rather than how to track state, this approach leads to simpler, more robust automation tools that can handle the unique challenges of VGAP.

As I continue to develop my Planets.nu automation tool, I'm exploring ways to extend this approach to handle more complex scenarios, such as fleet coordination and strategic decision-making. The Free Monad pattern provides a solid foundation for building these more advanced automation capabilities. 