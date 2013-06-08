{-# OPTIONS -Wall -Werror -Wwarn #-}

{- |
   Module     : Kurt.MainLoop
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental

this module runs the main gtp handling loop via startLoop.

-}


module Kurt.MainLoop ( startLoop
                     ) where


import           Control.Arrow                         ((&&&))
import           Data.List
import qualified Data.Map                              as M (assocs)
import           System.IO
import           Text.Parsec.String                    (Parser)
import           Text.Printf                           (printf)


import           Data.Goban.GameState                  (GameState (..),
                                                        GameStateStuff (..),
                                                        makeStonesAndLibertyHeuristic,
                                                        newGameState,
                                                        nextMoveColor,
                                                        nextMoves,
                                                        scoreGameState,
                                                        showGameState,
                                                        thisMoveColor)
import           Data.Goban.Incremental                (allStones)
import           Data.Goban.Types                      (Color (..), gtpShowMove,
                                                        gtpShowVertex,
                                                        isStoneMove, moveColor)
import           Data.Goban.Utils                      (influenceFromWinrate)
import           Network.GoTextProtocol2.Server.Parser
import           Network.GoTextProtocol2.Types

import           Data.Tree.UCT.GameMap                 (childNodes)
import           Data.Tree.UCT.Types                   (MoveNode (..),
                                                        newRaveMap)
import           Kurt.Config
import           Kurt.GoEngine                         (EngineState (..),
                                                        fakeMove, genMove,
                                                        newEngineState,
                                                        newUctTree,
                                                        simulatePlayout,
                                                        updateEngineState)

import           Debug.TraceOrId                       (trace)


type CommandHandler = [Argument] -> EngineState -> IO (Either String (String, EngineState))


lookupC :: String -> [(String, CommandHandler)] -> Maybe (String, CommandHandler)
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
    ,("kgs-game_over", noArgumentParser)
    ,("showboard", noArgumentParser)
    ,("time_left", timeleftArgParser)
    ,("time_settings", timesettingsArgParser)
    ,("version", noArgumentParser)

    ,("gogui-analyze_commands", noArgumentParser)

    ,("kurt_configure", maybeKeyValueArgParser)

    ,("kurt_uct_tree", noArgumentParser)
    ,("kurt_ravemap", noArgumentParser)
    ,("kurt_simulate_playout", noArgumentParser)

    ,("kurt_heuristic_total", noArgumentParser)
    ,("kurt_heuristic_capture", noArgumentParser)
    ,("kurt_heuristic_liberty_min", noArgumentParser)
    ,("kurt_heuristic_liberty_total", noArgumentParser)
    ,("kurt_heuristic_chain_count", noArgumentParser)
    ,("kurt_heuristic_center", noArgumentParser)
    ]



commandHandlers :: [(String, CommandHandler)]
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
    ,("kgs-game_over", cmd_quit)
    ,("showboard", cmd_showboard)
    ,("time_left", cmd_time_left)
    ,("time_settings", cmd_time_settings)
    ,("version", cmd_version)

    ,("gogui-analyze_commands", cmd_gogui_analyze_commands)

    ,("kurt_configure", cmd_kurt_configure)

    ,("kurt_uct_tree", cmd_kurt_uct_tree)
    ,("kurt_ravemap", cmd_kurt_ravemap)
    ,("kurt_simulate_playout", cmd_kurt_simulate_playout)

    ,("kurt_heuristic_total", cmd_kurt_heuristic_total)
    ,("kurt_heuristic_capture", make_cmd_kurt_heuristic (\c -> c { hCaptureWeight = 1 }))
    ,("kurt_heuristic_liberty_min", make_cmd_kurt_heuristic (\c -> c { hMinLibertiesWeight = 1 }))
    ,("kurt_heuristic_liberty_total", make_cmd_kurt_heuristic (\c -> c { hLibertiesWeight = 1 }))
    ,("kurt_heuristic_chain_count", make_cmd_kurt_heuristic (\c -> c { hChainCountWeight = 1 }))
    ,("kurt_heuristic_center", make_cmd_kurt_heuristic (\c -> c { hCenterWeight = 1 }))
    ]


startLoop :: KurtConfig -> IO ()
startLoop config =
    do
      hSetBuffering stdin LineBuffering
      hSetBuffering stdout LineBuffering
      loop $ newEngineState config

loop :: EngineState -> IO ()
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


cmd_known_command :: CommandHandler
cmd_known_command [StringArgument cmd] state =
    return $ case lookupC cmd commandHandlers of
      Nothing -> Right ("false", state)
      Just (_, _) -> Right ("true", state)
cmd_known_command _ _ = error "cmd_known_command called with illegal argument type"

cmd_list_commands :: CommandHandler
cmd_list_commands _ state =
    return $ Right (reverse $ drop 1 $ reverse $ unlines $ map fst commandHandlers, state)

cmd_name :: CommandHandler
cmd_name _ state =
    return $ Right ("kurt", state)

cmd_protocol_version :: CommandHandler
cmd_protocol_version _ state =
    return $ Right ("2", state)

cmd_quit :: CommandHandler
cmd_quit _ _ =
    error "bye!"

cmd_version :: CommandHandler
cmd_version _ state =
    return $ Right ("0.0.3", state)


cmd_clear_board :: CommandHandler
cmd_clear_board [] state =
  return $ Right ("", state { getGameState =
                                  newGameState (boardSize state) (getKomi state)
                            , getUctTree = newUctTree fakeMove
                            , getRaveMap = newRaveMap
                            })
cmd_clear_board _ _ = error "cmd_clear_board called with illegal argument type"

cmd_komi :: CommandHandler
cmd_komi [FloatArgument f] state =
    return $
    Right ("",
             state {
             getKomi = f
           , getGameState =
               gState { getState =
                            stuff { komi = f } } } )
    where
      gState = getGameState state
      stuff = getState gState

cmd_komi _ _ = error "cmd_komi called with illegal argument type"

cmd_boardsize :: CommandHandler
cmd_boardsize [IntArgument n] state =
    return $ Right ("",
                    state { getGameState = newGameState n (getKomi state)
                          , boardSize = n } )
cmd_boardsize _ _ = error "cmd_boardsize called with illegal argument type"

cmd_showboard :: CommandHandler
cmd_showboard [] state =
  return $ Right ("showboard\n" ++ (showGameState $ getGameState state), state)
cmd_showboard _ _ = error "cmd_showboard called with illegal argument type"

cmd_play :: CommandHandler
cmd_play [MoveArgument move] state =
    trace ("cmd_play " ++ gtpShowMove move ++ "\n" ++ str)
              $ return $ Right ("", state')
  where
    state' = updateEngineState state move
    str = showGameState $ getGameState state'
cmd_play _ _ = error "cmd_play called with illegal argument type"

cmd_genmove :: CommandHandler
cmd_genmove [ColorArgument color] state = do
  (move, state') <- genMove state color
  let state'' = updateEngineState state' move
  let str = showGameState $ getGameState state''
  trace ("cmd_genmove " ++ gtpShowMove move ++ "\n" ++ str)
            $ return $ Right (gtpShowMove move, state'')
cmd_genmove _ _ = error "cmd_genmove called with illegal argument type"

cmd_final_score :: CommandHandler
cmd_final_score [] state =
    return $ Right (scoreString scoreFloat, state)
    where
      scoreString s
          | s < 0 = "W+" ++ show (-1 * s)
          | s > 0 = "B+" ++ show s
          | otherwise = "0"
      scoreFloat = scoreGameState $ getGameState state
cmd_final_score _ _ = error "cmd_final_score called with illegal argument type"

cmd_final_status_list :: CommandHandler
cmd_final_status_list [StringArgument arg] state =
    return $ Right (str, state)
    where
      str = case arg of
              "dead" -> ""
              "seki" -> ""
              "alive" -> unwords $ map gtpShowVertex $ allStones $ chains $ getState $ getGameState state
              other -> error ("cmd_final_status_list illegal arg: " ++ other)
cmd_final_status_list _ _ = error "cmd_final_status_list called with illegal argument type"


cmd_time_left :: CommandHandler
cmd_time_left [TimeLeftArgument 0 0] state =
    return $ Right ("", state)
cmd_time_left [TimeLeftArgument seconds stones] state =
    return
    $ Right ("", state { getConfig = (getConfig state) { maxTime = ms'' } } )
    where
      ms'' = ms' - 300
      ms' =
          if stones == 0
          then ms `div` (movesLeft + 1)
          else ms `div` (stones + 1)
      ms = seconds * 950
      movesLeft = ((estMaxMoves - moveCount) * (estMaxMoves + moveCount))
                  `div`
                  ((4 * estMaxMoves) + 1)
      estMaxMoves = max ((boardSize state) ^ (2 :: Int)) (moveCount + 15)
      moveCount = length $ moveHistory $ getState $ getGameState state
cmd_time_left _ _ = error "cmd_time_left called with illegal argument type"

cmd_time_settings :: CommandHandler
cmd_time_settings [TimeSettingsArgument maintime byotime stones] state =
    return
    $ Right ("", state { getConfig = (getConfig state) { maxTime = ms'' } } )
    where
      ms'' = max 1 (ms' - 300)
      ms' = seconds * 1000
      seconds =
          if maintime > byotime
          then maintime `div` (movesLeft + 1)
          else byotime `div` (stones + 1)
      movesLeft = ((estMaxMoves - moveCount) * (estMaxMoves + moveCount))
                  `div`
                  ((4 * estMaxMoves) + 1)
      estMaxMoves = max ((boardSize state) ^ (2 :: Int)) (moveCount + 15)
      moveCount = length $ moveHistory $ getState $ getGameState state
cmd_time_settings _ _ = error "cmd_time_left called with illegal argument type"




-- gogui analyze gtp extensions
-------------------------------

cmd_gogui_analyze_commands :: CommandHandler
cmd_gogui_analyze_commands [] state =
    return $ Right (
           "param/kurt_configure/kurt_configure\n"
           ++ "gfx/kurt_uct_tree/kurt_uct_tree\n"
           ++ "gfx/kurt_ravemap/kurt_ravemap\n"
           ++ "var/kurt_simulate_playout/kurt_simulate_playout\n"
           ++ "gfx/kurt_heuristic_total/kurt_heuristic_total\n"
           ++ "gfx/kurt_heuristic_capture/kurt_heuristic_capture\n"
           ++ "gfx/kurt_heuristic_liberty_min/kurt_heuristic_liberty_min\n"
           ++ "gfx/kurt_heuristic_liberty_total/kurt_heuristic_liberty_total\n"
           ++ "gfx/kurt_heuristic_chain_count/kurt_heuristic_chain_count\n"
           ++ "gfx/kurt_heuristic_center/kurt_heuristic_center"
          , state)
cmd_gogui_analyze_commands _ _ = error "cmd_gogui_analyze_commands called with illegal argument type"


cmd_kurt_heuristic_total :: CommandHandler
cmd_kurt_heuristic_total [] state =
  make_cmd_kurt_heuristic (const (getConfig state)) [] state
cmd_kurt_heuristic_total _ _ = error "cmd_kurt_heuristic_total called with illegal argument type"

make_cmd_kurt_heuristic :: (KurtConfig -> KurtConfig) -> CommandHandler
make_cmd_kurt_heuristic fConfig [] state =
    return $ Right ("INFLUENCE" ++ str, state)
    where
      moves = nextMoves gState color
      slHeu = makeStonesAndLibertyHeuristic gState config'
      str = concatMap
            (\move
                 -> " " ++ gtpShowMove move
                    ++ printf " %.2f" (((fst $ slHeu move) - 0.5) * 2 * flipSig))
                    -- ++ printf " %.2f" (fst $ slHeu move))
            $ filter isStoneMove moves

      flipSig = if color == Black then 1 else -1
      color = nextMoveColor $ getState gState
      gState = getGameState state
      config' = fConfig $ (getConfig state) { hCaptureWeight       = 0
                                            , hMinLibertiesWeight  = 0
                                            , hLibertiesWeight     = 0
                                            , hChainCountWeight    = 0
                                            , hCenterWeight        = 0
                                            }

make_cmd_kurt_heuristic _ _ _ = error "make_cmd_kurt_heuristic called with illegal argument type"

cmd_kurt_uct_tree :: CommandHandler
cmd_kurt_uct_tree [] state =
    return $ Right (str ++ "\n" ++ str', state)
    where
      str = "INFLUENCE"
            ++ concatMap
                   (\(move, value)
                        -> " " ++ gtpShowMove move
                           ++ (influenceFromWinrate color value))
                   ucts

      str' = "LABEL"
             ++ concatMap
                    (\(move, v) -> " " ++ gtpShowMove move ++ " " ++ show v)
                    ucts'

      ucts = map (nodeMove &&& nodeValue) nodes
      ucts' = map (nodeMove &&& nodeVisits) nodes

      nodes = childNodes $ getUctTree state
      -- m = maximum $ map snd ucts

      color = thisMoveColor $ getState $ getGameState state

cmd_kurt_uct_tree _ _ = error "cmd_kurt_uct_tree called with illegal argument type"

cmd_kurt_ravemap :: CommandHandler
cmd_kurt_ravemap [] state =
    return $ Right (str ++ "\n" ++ str', state)
    where
      str = "INFLUENCE"
            ++ concatMap
                 (\(move, (value, _count)) ->
                      " " ++ gtpShowMove move
                      ++ (influenceFromWinrate color value))
                 raves

      str' = "LABEL"
            ++ concatMap
                 (\(move, (_value, count)) ->
                      " " ++ gtpShowMove move ++ " " ++ show count)
                 raves

      raves = filter ((== color) . moveColor . fst) $ M.assocs $ getRaveMap state
      color = thisMoveColor $ getState $ getGameState state
cmd_kurt_ravemap _ _ = error "cmd_kurt_ravemap called with illegal argument type"


cmd_kurt_simulate_playout :: CommandHandler
cmd_kurt_simulate_playout [] state = do
  moves <- simulatePlayout $ getGameState state
  let str = unwords $ map gtpShowMove moves
  return $ Right ("VAR " ++ str, state)

cmd_kurt_simulate_playout _ _ = error "cmd_kurt_ravemap called with illegal argument type"



-- purely kurt specific gtp extensions
--------------------------------------

cmd_kurt_configure :: CommandHandler
cmd_kurt_configure [MaybeKeyValueArgument Nothing] state =
    return $ Right (
                    init $ unlines [
                     "maxPlayouts " ++ show (maxPlayouts config)
                    ,"maxTime " ++ show (maxTime config)
                    ,"uctExplorationPercent " ++ show (uctExplorationPercent config)
                    ,"raveWeight " ++ show (raveWeight config)
                    ,"hCaptureWeight " ++ show (hCaptureWeight config)
                    ,"hMinLibertiesWeight " ++ show (hMinLibertiesWeight config)
                    ,"hLibertiesWeight " ++ show (hLibertiesWeight config)
                    ,"hChainCountWeight " ++ show (hChainCountWeight config)
                    ,"hCenterWeight " ++ show (hCenterWeight config)
                    ]
          , state)
    where
      config = getConfig state
cmd_kurt_configure [MaybeKeyValueArgument (Just (str, n))] state =
    return $ case str of
               "maxplayouts" -> Right ("maxPlayouts set to " ++ show n, state { getConfig = (getConfig state) { maxPlayouts = n } } )
               "maxtime" -> Right ("maxTime set to " ++ show n, state { getConfig = (getConfig state) { maxTime = n } })
               "uctexplorationpercent" -> Right ("uctExplorationPercent set to " ++ show n, state { getConfig = (getConfig state) { uctExplorationPercent = n } })
               "raveweight" -> Right ("raveWeight set to " ++ show n, state { getConfig = (getConfig state) { raveWeight = n} })
               "hcaptureweight" -> Right ("hCaptureWeight set to " ++ show n, state { getConfig = (getConfig state) { hCaptureWeight = n} })
               "hminlibertiesweight" -> Right ("hMinLibertiesWeight set to " ++ show n, state { getConfig = (getConfig state) { hMinLibertiesWeight = n} })
               "hlibertiesweight" -> Right ("hLibertiesWeight set to " ++ show n, state { getConfig = (getConfig state) { hLibertiesWeight = n} })
               "hchaincountweight" -> Right ("hChainCountWeight set to " ++ show n, state { getConfig = (getConfig state) { hChainCountWeight = n} })
               "hcenterweight" -> Right ("hCenterWeight set to " ++ show n, state { getConfig = (getConfig state) { hCenterWeight = n} })

               other -> Left ("unknown configuration key " ++ show other)
cmd_kurt_configure _ _ = error "cmd_kurt_configure called with illegal argument type"
