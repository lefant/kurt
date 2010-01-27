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


this module runs the main gtp handling loop via startLoop.

Written by Fabian Linzberger, e\@lefant.net
-}


module Network.GoTextProtocol2.Server (
                                       startLoop
                                      ) where
    

import Data.Char
import Data.List
import System.IO
import System.Random
import Text.Parsec.String (Parser)


import Network.GoTextProtocol2.Server.Parser
import Network.GoTextProtocol2.Server.Types
import Data.Goban.Utils
import Data.Goban (GameState(..), defaultGameState, updateGameState, score)
import Data.Goban.StoneList (defaultStoneListGoban)
import Kurt.Move (genMove)



type CommandHandler = [Argument] -> GameState -> Either String (String, GameState)

lookupC :: String -> [(String, CommandHandler)] -> Maybe (String, CommandHandler)
lookupC cmd cl = find (\(x, _) -> x == cmd) cl


commandargparserlist :: [([Char], Text.Parsec.String.Parser [Argument])]
commandargparserlist =
    [
     ("boardsize", intArgParser)
    ,("clear_board", noArgumentParser)
    ,("final_score", noArgumentParser)
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
    ,("final_score", cmd_final_score)
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
    do
      hSetBuffering stdin LineBuffering
      hSetBuffering stdout LineBuffering
      g <- newStdGen
      loop $ defaultGameState g

loop :: GameState -> IO ()
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
      -- hFlush stdout



outputIdOrBlank :: Maybe Id -> String
outputIdOrBlank Nothing = " "
outputIdOrBlank (Just lineId) = "[" ++ (show lineId) ++ "] "


cmd_known_command :: CommandHandler
cmd_known_command [(StringArgument cmd)] state =
    case lookupC cmd commandHandlers of
      Nothing -> Right ("false", state)
      Just (_, _) -> Right ("true", state)
cmd_known_command _ _ = error "cmd_known_command called with illegal argument type"

cmd_list_commands :: CommandHandler
cmd_list_commands _ state =
    Right ((reverse $ drop 1 $ reverse $ unlines $ map fst commandHandlers), state)

cmd_name :: CommandHandler
cmd_name _ state =
    Right ("kurt", state)

cmd_protocol_version :: CommandHandler
cmd_protocol_version _ state =
    Right ("2", state)

cmd_quit :: CommandHandler
cmd_quit _ _ =
    error "bye!"

cmd_version :: CommandHandler
cmd_version _ state =
    Right ("0.0.1", state)


cmd_clear_board :: CommandHandler
cmd_clear_board [] state =
    Right ("", state {
                  goban = clearGoban (goban state)
                 ,toMove = Black
                 ,moveHistory = []
                 ,koBlocked = []
                 ,blackPrisoners = 0
                 ,whitePrisoners = 0
                 })
cmd_clear_board _ _ = error "cmd_clear_board called with illegal argument type"

cmd_komi :: CommandHandler
cmd_komi [(FloatArgument f)] state =
    Right ("", state { komi = f })
cmd_komi _ _ = error "cmd_komi called with illegal argument type"

cmd_boardsize :: CommandHandler
cmd_boardsize [(IntArgument n)] state =
    Right ("", state { goban = (defaultStoneListGoban n) })
cmd_boardsize _ _ = error "cmd_boardsize called with illegal argument type"

cmd_showboard :: CommandHandler
cmd_showboard [] state =
    Right ("showboard received: " ++ (show state), state)
cmd_showboard _ _ = error "cmd_showboard called with illegal argument type"

cmd_play :: CommandHandler
cmd_play [(MoveArgument move)] state =
    Right ("", updateGameState state move)
cmd_play _ _ = error "cmd_play called with illegal argument type"

cmd_genmove :: CommandHandler
cmd_genmove [(ColorArgument color)] state =
    if (toMove state) /= color
    then error "state and genmove parameter disagree about color to move next"
    else Right (show move, state')
    where
      state' = updateGameState state { ourRandomGen = g' } move
      -- move = genMoveRand state { ourRandomGen = g }
      move = genMove state { ourRandomGen = g }
      (g, g') = split (ourRandomGen state)
cmd_genmove _ _ = error "cmd_genmove called with illegal argument type"

cmd_final_score :: CommandHandler
cmd_final_score [] state =
    Right (scoreString scoreFloat, state)
    where
      scoreString s
          | s < 0 = "W+" ++ (show (-1 * s))
          | s > 0 = "B+" ++ (show s)
          | otherwise = "0"
      scoreFloat = (score state)
cmd_final_score _ _ = error "cmd_final_score called with illegal argument type"



-- TODO

cmd_time_left :: CommandHandler
cmd_time_left [(IntArgument n)] state =
    Right ("time left: " ++ (show n), state)
cmd_time_left _ _ = error "cmd_time_left called with illegal argument type"

