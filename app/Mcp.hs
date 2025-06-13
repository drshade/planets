{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}


module Mcp where

import           Api                        (currentTurn, login)
import           Config                     (readCredential)
import           Data.Aeson                 (decode, encode)
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LChar8 (pack, unpack)
import qualified Data.Text                  as T
import qualified GHC.IO.Device              as LBS
import           MCP.Server
import           Model
import           Optics
import           System.IO.Error            (tryIOError)

data McpTool
    = GetGameProperties
    | GetMyPlanets
    | GetMyShips
    | GetPlanets { x :: Int, y :: Int, radius :: Int }
    | GetShips { x :: Int, y :: Int, radius :: Int }
    deriving (Show, Eq)

myGameId :: String
myGameId = "643598"

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

insideCircle :: Int -> Int -> Int -> Position -> Bool
insideCircle x y radius pos =
    pos ^. positionX >= (x - radius)
    && pos ^. positionX <= (x + radius)
    && pos ^. positionY >= (y - radius)
    && pos ^. positionY <= (y + radius)
