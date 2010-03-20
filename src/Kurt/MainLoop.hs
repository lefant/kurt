{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{- |
   Module     : Network.GoTextProtocol2.Server
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

this module runs the main gtp handling loop via startLoop.

-}


module Kurt.MainLoop ( startLoop
                     ) where
    

import Control.Monad.ST (stToIO, RealWorld)
import System.IO
import Text.Parsec.String (Parser)
import Data.Char
import Data.List
import Text.Printf (printf)


import Network.GoTextProtocol2.Server.Parser
import Network.GoTextProtocol2.Types
import Data.Goban.GameState (GameState(..), newGameState, showGameState, updateGameState, scoreGameState, makeStonesAndLibertyHeuristic, nextMoves, nextMoveColor)
import Data.Goban.Types (gtpShowMove, gtpShowVertex, Move(..), Stone(..))
import Data.Goban.STVectorGoban (allStones)

import Kurt.GoEngine (EngineState(..), newEngineState, genMove, heuristicWeights)


import Debug.TraceOrId (trace)


type CommandHandler s = [Argument] -> EngineState s -> IO (Either String (String, EngineState s))


lookupC :: String -> [(String, CommandHandler RealWorld)] -> Maybe (String, CommandHandler RealWorld)
lookupC cmd = find (\(x, _) -> x == cmd)


commandargparserlist :: [(String, Text.Parsec.String.Parser [Argument])]
commandargparserlist =
    [
     ("boardsize", intArgParser)
    ,("clear_board", noArgumentParser)
    ,("final_score", noArgumentParser)
    ,("final_status_list", stringArgParser)
    ,("genmove", colorArgParser)
    ,("known_command", stringArgParser)
    ,("komi", floatArgParser)
    ,("list_commands", noArgumentParser)
    ,("name", noArgumentParser)
    ,("play", moveArgParser)
    ,("protocol_version", noArgumentParser)
    ,("quit", noArgumentParser)
    ,("showboard", noArgumentParser)
    ,("time_left", timeleftArgParser)
    ,("version", noArgumentParser)
    ,("kurt_simuls", intArgParser)
    ,("gogui-analyze_commands", noArgumentParser)
    ,("kurt_uct_tree", noArgumentParser)
    ,("kurt_ravemap", noArgumentParser)
    ,("kurt_heuristic", noArgumentParser)
    ,("kurt_heuristic_stone", noArgumentParser)
    ,("kurt_heuristic_libertyMin", noArgumentParser)
    ,("kurt_heuristic_libertyAvg", noArgumentParser)
    ,("kurt_heuristic_center", noArgumentParser)
    ]



commandHandlers :: [(String, CommandHandler RealWorld)]
commandHandlers =
    [
     ("boardsize", cmd_boardsize)
    ,("clear_board", cmd_clear_board)
    ,("final_score", cmd_final_score)
    ,("final_status_list", cmd_final_status_list)
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
    ,("kurt_simuls", cmd_kurt_simuls)
    ,("gogui-analyze_commands", cmd_gogui_analyze_commands)
    ,("kurt_heuristic", cmd_kurt_heuristic)
    -- ,("kurt_heuristic_stone", cmd_kurt_heuristic_stone)
    -- ,("kurt_heuristic_libertyMin", cmd_kurt_heuristic_libertyMin)
    -- ,("kurt_heuristic_libertyAvg", cmd_kurt_heuristic_libertyAvg)
    -- ,("kurt_heuristic_center", cmd_kurt_heuristic_center)
    -- ,("kurt_uct_tree", cmd_kurt_uct_tree)
    -- ,("kurt_ravemap", cmd_kurt_ravemap)
    ]


startLoop :: IO ()
startLoop =
    do
      hSetBuffering stdin LineBuffering
      hSetBuffering stdout LineBuffering
      stToIO newEngineState >>= loop

loop :: EngineState RealWorld -> IO ()
loop oldState =
    do
      input <- getLine
      let parseResult = pureParseCommand input commandargparserlist
      case parseResult of
        Left err ->
            do
              putStrLn $ "? Couldn't parse command: " ++ show err
              putStrLn ""
              loop oldState
        Right (maybeId, Command cmd args) ->
              case lookupC cmd commandHandlers of
                Nothing ->
                    do
                      putStrLn $ "?" ++ outputIdOrBlank maybeId ++ "Unrecognized command " ++ cmd
                      newLineFlush
                      loop oldState
                Just (_, handler) ->
                    do
                      result <- handler args oldState
                      case result of
                        Left err ->
                            do
                              putStrLn $ "?" ++ outputIdOrBlank maybeId ++ " error: " ++ err
                              newLineFlush
                              loop oldState
                        Right (msg, newState) ->
                            do
                              putStrLn $ "=" ++ outputIdOrBlank maybeId ++ msg
                              newLineFlush
                              loop newState

newLineFlush :: IO ()
newLineFlush =
    putStrLn ""


outputIdOrBlank :: Maybe Id -> String
outputIdOrBlank Nothing = " "
outputIdOrBlank (Just lineId) = show lineId ++ " "


cmd_known_command :: CommandHandler RealWorld
cmd_known_command [StringArgument cmd] state =
    return $ case lookupC cmd commandHandlers of
      Nothing -> Right ("false", state)
      Just (_, _) -> Right ("true", state)
cmd_known_command _ _ = error "cmd_known_command called with illegal argument type"

cmd_list_commands :: CommandHandler RealWorld
cmd_list_commands _ state =
    return $ Right (reverse $ drop 1 $ reverse $ unlines $ map fst commandHandlers, state)

cmd_name :: CommandHandler RealWorld
cmd_name _ state =
    return $ Right ("kurt", state)

cmd_protocol_version :: CommandHandler RealWorld
cmd_protocol_version _ state =
    return $ Right ("2", state)

cmd_quit :: CommandHandler RealWorld
cmd_quit _ _ =
    error "bye!"

cmd_version :: CommandHandler RealWorld
cmd_version _ state =
    return $ Right ("0.0.1", state)


cmd_clear_board :: CommandHandler RealWorld
cmd_clear_board [] state = do
  gState' <- stToIO $ newGameState (boardSize state) (getKomi state)
  return $ Right ("", state { getGameState = gState' })
cmd_clear_board _ _ = error "cmd_clear_board called with illegal argument type"

cmd_komi :: CommandHandler RealWorld
cmd_komi [FloatArgument f] state =
    return $ Right ("",
                    state {
                      getKomi = f
                    , getGameState = gState { komi = f } } )
    where
      gState = getGameState state
cmd_komi _ _ = error "cmd_komi called with illegal argument type"

cmd_boardsize :: CommandHandler RealWorld
cmd_boardsize [IntArgument n] state =
    return $ Right ("",
                    state {
                      getGameState =
                      error "gamestate undefined after boardsize until clear board received"
                    , boardSize = n } )
cmd_boardsize _ _ = error "cmd_boardsize called with illegal argument type"

cmd_showboard :: CommandHandler RealWorld
cmd_showboard [] state = do
    str <- stToIO $ showGameState $ getGameState state
    return $ Right ("showboard" ++ str, state)
cmd_showboard _ _ = error "cmd_showboard called with illegal argument type"

cmd_play :: CommandHandler RealWorld
cmd_play [MoveArgument move] state = do
  gState' <- stToIO $ updateGameState (getGameState state) move
  str <- stToIO $ showGameState $ getGameState state
  trace ("cmd_play board: " ++ str) $ return ()
  score <- stToIO $ scoreGameState gState'
  trace ("cmd_play score: " ++ show score) $ return ()
  return $ Right ("", state { getGameState = gState' })
cmd_play _ _ = error "cmd_play called with illegal argument type"

cmd_genmove :: CommandHandler RealWorld
cmd_genmove [ColorArgument color] state = do
  move <- genMove state color
  gState' <- stToIO $ updateGameState (getGameState state) move
  str <- stToIO $ showGameState gState'
  trace ("cmd_genmove board:" ++ str) $ return ()
  score <- stToIO $ scoreGameState gState'
  trace ("cmd_genmove score: " ++ show score) $ return ()
  return $ Right (gtpShowMove move, state { getGameState = gState' })
cmd_genmove _ _ = error "cmd_genmove called with illegal argument type"

cmd_final_score :: CommandHandler RealWorld
cmd_final_score [] state = do
  scoreFloat <- stToIO $ scoreGameState $ getGameState state
  return $ Right (scoreString scoreFloat, state)
    where
      scoreString s
          | s < 0 = "W+" ++ show (-1 * s)
          | s > 0 = "B+" ++ show s
          | otherwise = "0"
cmd_final_score _ _ = error "cmd_final_score called with illegal argument type"

cmd_final_status_list :: CommandHandler RealWorld
cmd_final_status_list [StringArgument arg] state =
    case arg of
      "dead" -> return $ Right ("", state)
      "seki" -> return $ Right ("", state)
      "alive" ->
          do
            stones <- stToIO $ allStones $ goban $ getGameState state
            return $ Right (unwords $ map gtpShowVertex stones, state)
      other -> error ("cmd_final_status_list illegal arg: " ++ other)
cmd_final_status_list _ _ = error "cmd_final_status_list called with illegal argument type"


cmd_time_left :: CommandHandler RealWorld
cmd_time_left [TimeLeftArgument (seconds, stones)] state =
    return
    $ Right ("", state { timePerMove = milliseconds } )
    where
      milliseconds =
          if stones == 0
          then (seconds * 900) `div` estMaxMoves
          else (seconds * 900) `div` stones
      estMaxMoves = (boardSize state + 1) ^ (2 :: Int)
cmd_time_left _ _ = error "cmd_time_left called with illegal argument type"




-- gogui analyze gtp extensions
-------------------------------

cmd_gogui_analyze_commands :: CommandHandler RealWorld
cmd_gogui_analyze_commands [] state =
    return $ Right (
           "gfx/kurt_uct_tree/kurt_uct_tree\n"
           ++ "gfx/kurt_ravemap/kurt_ravemap\n"
           ++ "gfx/kurt_heuristic/kurt_heuristic\n"
           ++ "gfx/kurt_heuristic_stone/kurt_heuristic_stone\n"
           ++ "gfx/kurt_heuristic_libertyMin/kurt_heuristic_libertyMin\n"
           ++ "gfx/kurt_heuristic_libertyAvg/kurt_heuristic_libertyAvg\n"
           ++ "gfx/kurt_heuristic_center/kurt_heuristic_center"
          , state)
cmd_gogui_analyze_commands _ _ = error "cmd_gogui_analyze_commands called with illegal argument type"


-- INFLUENCE a7 -1 b7 -0.75 c7 -0.5 d7 -0.25 e7 0 f7 0.25 g7 0.5 h7 0.75 j7 1
cmd_kurt_heuristic :: CommandHandler RealWorld
cmd_kurt_heuristic [] state = do
  moves <- stToIO $ nextMoves gState color
  slHeu <- stToIO $ makeStonesAndLibertyHeuristic gState heuristicWeights
  let str = concatMap
            (\move@(Move (Stone p _c))
                 -> " " ++ gtpShowVertex p
                    ++ printf " %.2f" (((fst $ slHeu move) - 0.5) * 2))
            moves
  return $ Right ("INFLUENCE" ++ str, state)
    where
      color = nextMoveColor gState
      gState = getGameState state
cmd_kurt_heuristic _ _ = error "cmd_kurt_uct_debug called with illegal argument type"


-- cmd_kurt_uct_debug :: CommandHandler RealWorld
-- cmd_kurt_uct_debug [] state =
--     return $ Right ("uctDebug disabled", state)
--     -- where
--     --   gfxString = uctDebug state
-- cmd_kurt_uct_debug _ _ = error "cmd_kurt_uct_debug called with illegal argument type"




-- purely kurt specific gtp extensions
--------------------------------------

cmd_kurt_simuls :: CommandHandler RealWorld
cmd_kurt_simuls [IntArgument n] state =
    return $ Right ("", state { simulCount = n })
cmd_kurt_simuls _ _ = error "cmd_kurt_simuls called with illegal argument type"

