{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{-
Copyright (C) 2010 Fabian Linzberger <e@lefant.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
-}

{- |
   Module     : Network.GoTextProtocol2.Server
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

initial stub command handlers directly here

TODO:

 * merge commandargparserlist and commandHandlers lists implement a
 * way so commandHandlers can be passed in from an inheriting module
   running startLoop


Written by Fabian Linzberger, e\@lefant.net
-}


module Network.GoTextProtocol2.Server (
                                       startLoop
                                      ) where
    
import Network.GoTextProtocol2.Server.Parser
import Network.GoTextProtocol2.Server.Types

import Data.Char
import Data.List
import System.IO


data State = State Board History Komi
             deriving (Show)

type CommandHandler = [Argument] -> State -> Either String (String, State)

lookupC :: String -> [(String, CommandHandler)] -> Maybe (String, CommandHandler)
lookupC cmd cl = find (\(x, _) -> x == cmd) cl



commandargparserlist =
    [
     ("boardsize", intArgParser)
    ,("clear_board", noArgumentParser)
    ,("genmove", colorArgParser)
    ,("known_command", stringArgParser)
    ,("komi", floatArgParser)
    ,("list_commands", noArgumentParser)
    ,("name", noArgumentParser)
    ,("play", moveArgParser)
    ,("protocol_version", noArgumentParser)
    ,("quit", noArgumentParser)
    ,("showboard", noArgumentParser)
    ,("time_left", intArgParser)
    ,("version", noArgumentParser)
    ]



commandHandlers :: [(String, CommandHandler)]
commandHandlers =
    [
     ("boardsize", cmd_boardsize)
    ,("clear_board", cmd_clear_board)
    ,("genmove", cmd_genmove)
    ,("known_command", cmd_known_command)
    ,("komi", cmd_komi)
    ,("list_commands", cmd_list_commands)
    ,("name", cmd_name)
    ,("play", cmd_play)
    ,("protocol_version", cmd_protocol_version)
    ,("quit", cmd_quit)
    ,("showboard", cmd_showboard)
    ,("time_left", cmd_time_left)
    ,("version", cmd_version)
    ]


startLoop :: IO ()
startLoop =
    loop (State (Board 1 []) [] 0)

loop :: State -> IO ()
loop oldState =
    do
      input <- getLine
      parseResult <- return $ pureParseCommand input commandargparserlist
      case parseResult of
        Left err ->
            do
              putStrLn $ "? Couldn't parse command: " ++ (show err)
              putStrLn ""
              loop oldState
        Right (maybeId, Command cmd args) ->
            do
              case lookupC cmd commandHandlers of
                Nothing ->
                    do
                      putStrLn $ "?" ++ (outputIdOrBlank maybeId) ++ "Unrecognized command " ++ cmd
                      newLineFlush
                      loop oldState
                Just (_, handler) ->
                    do
                      result <- return $ handler args oldState
                      case result of
                        Left err ->
                            do
                              putStrLn $ "?" ++ (outputIdOrBlank maybeId) ++ " error: " ++ err
                              newLineFlush
                              loop oldState
                        Right (msg, newState) ->
                            do
                              putStrLn $ "=" ++ (outputIdOrBlank maybeId) ++ msg
                              newLineFlush
                              loop newState

newLineFlush :: IO ()
newLineFlush =
    do
      putStrLn ""
      hFlush stdout



outputIdOrBlank :: Maybe Id -> String
outputIdOrBlank Nothing = " "
outputIdOrBlank (Just lineId) = "[" ++ (show lineId) ++ "] "


cmd_known_command :: CommandHandler
cmd_known_command [(StringArgument cmd)] state =
    case lookupC cmd commandHandlers of
      Nothing -> Right ("false", state)
      Just (_, _) -> Right ("true", state)

cmd_list_commands :: CommandHandler
cmd_list_commands [] state =
    Right ((reverse $ drop 1 $ reverse $ unlines $ map fst commandHandlers), state)

cmd_name :: CommandHandler
cmd_name [] state =
    Right ("kurt", state)

cmd_protocol_version :: CommandHandler
cmd_protocol_version [] state =
    Right ("2", state)

cmd_quit :: CommandHandler
cmd_quit [] _ =
    error "bye!"

cmd_version :: CommandHandler
cmd_version [] state =
    Right ("0.0.1", state)



cmd_clear_board :: CommandHandler
cmd_clear_board [] (State (Board size _) _ komi) =
    Right ("", State (Board size []) [] komi)

cmd_komi :: CommandHandler
cmd_komi [(FloatArgument komi)] (State board history _komi) =
    Right ("", State board history komi)

cmd_boardsize :: CommandHandler
cmd_boardsize [(IntArgument n)] (State _board history komi) =
    Right ("", State (Board n []) history komi)

cmd_showboard :: CommandHandler
cmd_showboard [] state =
    Right ("showboard received: " ++ (show state), state)


cmd_play :: CommandHandler
cmd_play [(MoveArgument move)] (State oldBoard@(Board n board) history komi) =
    case move of
      (color, Nothing) -> Right ("", State oldBoard newHistory komi)
      (color, Just vertex) ->
          case lookupVertex vertex board of
            Nothing -> Right ("", State (Board n ((vertex, color) : board)) newHistory komi)
            Just stone -> Left ("there all ready is a stone: " ++ (show stone))
    where
      newHistory = (history ++ [move])

-- TODO

cmd_time_left :: CommandHandler
cmd_time_left [(IntArgument n)] state =
    Right ("time left: " ++ (show n), state)


cmd_genmove :: CommandHandler
cmd_genmove [(ColorArgument color)] (State oldBoard@(Board n board) history komi) =
    Right ("pass", State oldBoard newHistory komi)
    where
      newHistory = (history ++ [(color, Nothing)])



lookupVertex :: Vertex -> [(Vertex, Color)] -> Maybe (Vertex, Color)
lookupVertex vertex board =
    find (\(x, _) -> x == vertex) board
