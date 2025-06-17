{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}


module Mcp where

import           Api                  (currentTurn, login)
import           Config               (readCredential)
import           Data.Aeson           (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           Guide.Beginner       (beginnerGuide)
import           Guide.Combat         (combatGuide)
import           Guide.Economics      (economicsGuide)
import           Guide.FriendlyCodes  (friendlyCodesGuide)
import           Guide.FullGuide      (fullGameGuideContent)
import           Guide.Homeworlds     (homeworldsGuide)
import           Guide.Minefields     (minefieldsGuide)
import           Guide.Navigation     (navigationGuide)
import           Guide.Overall        (gameGuideContent)
import           Guide.Races          (racesGuide)
import           Guide.Ships          (shipsGuide)
import           Guide.Starbases      (starbasesGuide)
import           Guide.Victory        (victoryGuide)
import           MCP.Server
import           Model
import           Network.URI          (URI)
import           Optics
import           System.IO.Error      (tryIOError)

data McpResource
    = GameGuide
    | RacesGuide
    | ShipsGuide
    | MinefieldsGuide
    | EconomicsGuide
    | StarbasesGuide
    | CombatGuide
    | NavigationGuide
    | FriendlyCodesGuide
    | VictoryGuide
    | BeginnerGuide
    | HomeworldsGuide
    deriving (Show, Eq)

data McpTool
    = GetGameProperties
    | GetGameGuide
    | GetMyPlanets
    | GetMyShips
    | GetPlanets { x :: Int, y :: Int, radius :: Int }
    | GetShips { x :: Int, y :: Int, radius :: Int }
    deriving (Show, Eq)

data McpPrompt
    = DraftCommunication
    deriving (Show, Eq)

myGameId :: String
-- myGameId = "643598"
myGameId = "641474"

getGamestate :: IO Gamestate
getGamestate = do
    -- Do we already have a turn on the disk?
    -- Check if file exists?
    cache <- tryIOError $ LBS.readFile "cache.turn"
    turn <- case cache of
        Left _ -> do
            (username, password) <- readCredential
            apikey <- login username password
            turn <- currentTurn apikey myGameId
            LBS.writeFile "cache.turn" (encode turn)
            pure turn
        Right contents -> do
            case decode contents of
                Nothing   -> error $ "Failed to parse cached"
                Just turn -> pure turn

    pure $ Model.fromLoadTurnResponse turn

insideCircle :: Int -> Int -> Int -> Position -> Bool
insideCircle x y radius pos =
    pos ^. positionX >= (x - radius)
    && pos ^. positionX <= (x + radius)
    && pos ^. positionY >= (y - radius)
    && pos ^. positionY <= (y + radius)

handleTool :: McpTool -> IO Content
handleTool GetGameProperties = do
    gamestate <- getGamestate
    let output = "Gamestate:\n"
                    <> show (gamestate ^. gamestateGame)
                    <> "Current Player: \n"
                    <> show (gamestate ^. gamestatePlayer)
                    <> "Players:\n"
                    <> show (gamestate ^. gamestatePlayers)
    pure $ ContentText $ T.pack output

handleTool GetMyPlanets = do
    gamestate <- getGamestate
    let currentPlayer = gamestate ^. gamestatePlayer ^. playerId
    let planets = filter (\planet -> planet ^. planetOwnerId == currentPlayer) $ gamestate ^. gamestatePlanets
    pure $ ContentText $ T.pack $ show planets

handleTool GetMyShips = do
    gamestate <- getGamestate
    let currentPlayer = gamestate ^. gamestatePlayer ^. playerId
    let ships = filter (\ship -> ship ^. shipOwnerId == currentPlayer) $ gamestate ^. gamestateShips
    pure $ ContentText $ T.pack $ show ships

handleTool (GetPlanets {..}) = do
    gamestate <- getGamestate
    let planets = filter (\planet -> insideCircle x y radius (planet ^. planetPosition) ) $ gamestate ^. gamestatePlanets
    pure $ ContentText $ T.pack $ show planets

handleTool (GetShips {..}) = do
    gamestate <- getGamestate
    let ships = filter (\ship -> insideCircle x y radius (ship ^. shipPosition) ) $ gamestate ^. gamestateShips
    pure $ ContentText $ T.pack $ show ships

handleTool GetGameGuide = do
    pure $ ContentText $ T.pack fullGameGuideContent

handleResource :: URI -> McpResource -> IO ResourceContent
handleResource uri GameGuide = do
    pure $ ResourceText uri "text/plain" $ T.pack gameGuideContent

handleResource uri RacesGuide = do
    pure $ ResourceText uri "text/plain" $ T.pack racesGuide

handleResource uri ShipsGuide = do
    pure $ ResourceText uri "text/plain" $ T.pack shipsGuide

handleResource uri MinefieldsGuide = do
    pure $ ResourceText uri "text/plain" $ T.pack minefieldsGuide

handleResource uri EconomicsGuide = do
    pure $ ResourceText uri "text/plain" $ T.pack economicsGuide

handleResource uri StarbasesGuide = do
    pure $ ResourceText uri "text/plain" $ T.pack starbasesGuide

handleResource uri CombatGuide = do
    pure $ ResourceText uri "text/plain" $ T.pack combatGuide

handleResource uri NavigationGuide = do
    pure $ ResourceText uri "text/plain" $ T.pack navigationGuide

handleResource uri FriendlyCodesGuide = do
    pure $ ResourceText uri "text/plain" $ T.pack friendlyCodesGuide

handleResource uri VictoryGuide = do
    pure $ ResourceText uri "text/plain" $ T.pack victoryGuide

handleResource uri BeginnerGuide = do
    pure $ ResourceText uri "text/plain" $ T.pack beginnerGuide

handleResource uri HomeworldsGuide = do
    pure $ ResourceText uri "text/plain" $ T.pack homeworldsGuide


handlePrompt :: McpPrompt -> IO Content
handlePrompt DraftCommunication = do
    gamestate <- getGamestate
    let currentPlayer = gamestate ^. gamestatePlayer
    let currentRace = currentPlayer ^. playerRace
    let otherPlayers = filter (\p -> p ^. playerId /= currentPlayer ^. playerId) (gamestate ^. gamestatePlayers)
    let gameturn = gamestate ^. gamestateGame ^. gameTurn
    let myPlanetCount = length $ filter (\p -> p ^. planetOwnerId == currentPlayer ^. playerId) (gamestate ^. gamestatePlanets)
    let myShipCount = length $ filter (\s -> s ^. shipOwnerId == currentPlayer ^. playerId) (gamestate ^. gamestateShips)

    let raceVoice = getRaceVoice currentRace
    let contextInfo = "Current game state context:\n"
                   <> "- Turn: " <> show gameturn <> "\n"
                   <> "- Your race: " <> show currentRace <> "\n"
                   <> "- Your planets: " <> show myPlanetCount <> "\n"
                   <> "- Your ships: " <> show myShipCount <> "\n"
                   <> "- Other players: " <> show (length otherPlayers) <> "\n\n"

    let prompt = contextInfo <> raceVoice <> "\n\n"
              <> "Draft a short but punchy communication to each of the other players in the game. "
              <> "Address each player individually, taking into account:\n"
              <> "- The strategic situation and current game state\n"
              <> "- Your race's personality and communication style\n"
              <> "- Potential threats, alliances, or trade opportunities\n"
              <> "- Keep each message concise but impactful (3-5 sentences)\n\n"
              <> "- Don't be too serious - but rather sarcastic, funny or quirky.\n\n"
              <> "Other players to address:\n"
              <> unlines (map (\p -> "- " <> p ^. playerUsername <> " (" <> show (p ^. playerRace) <> ")") otherPlayers)

    pure $ ContentText $ T.pack prompt
    where
        getRaceVoice :: Race -> String
        getRaceVoice race = case race of
            Feds -> "As the Solar Federation, you embody diplomacy, honor, and peaceful expansion. Your communications are formal but respectful, emphasizing cooperation and mutual benefit. You believe in the rule of law and fair play."
            Lizards -> "As the Lizard Alliance, you are cunning, patient, and opportunistic. Your messages are calculated and sometimes contain veiled threats. You prefer indirect approaches and are masters of timing."
            Birds -> "As the Bird Men, you are proud, territorial, and value strength. Your communications are direct and sometimes aggressive. You respect power and despise weakness."
            Hoards -> "As the Fascist Empire, you are authoritarian, expansionist, and ruthless. Your messages are commanding and intimidating. You believe might makes right and brook no defiance."
            Privateers -> "As the Privateer Bands, you are mercenary, profit-driven, and pragmatic. Your communications focus on deals, trades, and mutual profit. Everything has a price."
            Cyborgs -> "As the Cyborg, you are logical, efficient, and coldly calculating. Your messages are precise and data-driven. Emotion is irrelevant; only results matter."
            Crystals -> "As the Crystal Confederation, you are ancient, wise, and energy-focused. Your communications have an otherworldly quality, speaking of cosmic balance and long-term consequences."
            Empire -> "As the Evil Empire, you are dark, manipulative, and power-hungry. Your messages drip with malice and hidden agendas. You enjoy psychological warfare."
            Robots -> T.unpack $ T.toUpper $ "As the Robotic Imperium, you are mechanical, systematic, and relentlessly logical, but sometimes qqquiiiiiiiiirk-eeeey. [[..set_mode(humour::/disabled)]]Your communic-cccccccc-ations are structured and binary. Inefficiency must be eliminat-ttttttttt-ed. Ttttt-ED. [[..embed emotions as code in transmissions..]] ::// MESSAGE::end()"
            Rebels -> "As the Rebel Confederation, you are scrappy, resourceful, and fiercely independent. Your messages are informal but passionate. You fight for freedom against oppression."
            Colonies -> "As the Missing Colonies of Man, you are survivors, adaptable, and community-focused. Your communications emphasize unity and mutual aid in the face of adversity."
            Plague -> "As the Virus, you are parasitic, spreading, and consuming. Your communications have an unsettling quality, speaking of assimilation and inevitable consumption."
