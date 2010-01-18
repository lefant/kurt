{-# OPTIONS -O2 -Wall -Werror -Wwarn -XRankNTypes #-}

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
   Module     : Network.GoTextProtocol2.Server.Parser
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Parsing GTP protocol messages

TODO:

 * handle #-prefix comments and empty lines
 * finish parsing expected arguments


3.1 Preprocessing

When a command string arrives to an engine, it is expected to perform the following four operations before any further parsing takes place:

   1. Remove all occurences of CR and other control characters except for HT and LF.
   2. For each line with a hash sign (#), remove all text following and including this character.
   3. Convert all occurences of HT to SPACE.
   4. Discard any empty or white-space only lines.

When a response arrives to a controller, it is expected only to do steps 1 and 3 above. 

-}

module Network.GoTextProtocol2.Server.Parser (
                                              parseCommand
                                             ,noArgumentParser
                                             ,intArgParser
                                             ,floatArgParser
                                             ,colorArgParser
                                             ,moveArgParser
                                             ,stringArgParser
                                             ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.IO(Handle,hGetLine)
import Data.Char
import Monad

import Network.GoTextProtocol2.Server.Types


type CommandArgParserList = [(String, Parser [Argument])]


parseCommand :: Handle -> CommandArgParserList -> IO (Either ParseError (Maybe Id, Command))
parseCommand h commandargparserlist =
    do input <- hGetLine h
       return $ parse (line commandargparserlist) "(unknown)" input



line :: CommandArgParserList -> Parser (Maybe Id, Command)
line commandList =
    do
      {- FIXME: more work here
      skipMany (do
                 emptyline
             <|> commentline)
       -}
      (idCommandline commandList)
  <|> (commandline commandList)
  <?> "command"


emptyline :: Parser ()
emptyline =
    do
      spaces

commentline :: Parser ()
commentline =
    do
      char '#'
      return ()
      -- FIXME: need to do some more work here


commandline :: CommandArgParserList -> Parser (Maybe Id, Command)
commandline commandList =
    do
      c <- (command commandList)
      spaces
      return (Nothing, c)


idCommandline :: CommandArgParserList -> Parser (Maybe Id, Command)
idCommandline commandList =
    do
      lineId <- commandId
      space
      spaces
      c <- (command commandList)
      spaces
      return (Just lineId, c)

commandId :: Parser Int
commandId =
    do
      char '['
      i <- parseNumber
      char ']'
      return i





command :: CommandArgParserList -> Parser Command
command commandList =
    choice $ map command2parser commandList



command2parser :: (String, Parser [Argument]) -> Parser Command
command2parser (str, argParser) =
    try
    (do
      cmd <- string str
      args <- argParser
      return (Command cmd args))






noArgumentParser :: Parser [Argument]
noArgumentParser = return []


stringArgParser :: Parser [Argument]
stringArgParser = undefined
commandString :: Parser String
commandString =
    do
      str <- many1 (letter <|> char '-' <|> char '_')
      return (map toLower str)



floatArgParser :: Parser [Argument]
floatArgParser = undefined

moveArgParser :: Parser [Argument]
moveArgParser = undefined

colorArgParser :: Parser [Argument]
colorArgParser = undefined



intArgParser :: Parser [Argument]
intArgParser =
    do
      space
      spaces
      n <- parseNumber
      return $ [IntArgument n]

parseNumber :: Parser Int
parseNumber = liftM read $ many1 digit
