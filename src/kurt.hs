{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- |
   Module     : Main
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental

This is the actual entry point for running the program. Command line
option processing happens here, then Kurt.MainLoop.loop is launched
via Kurt.MainLoop.startLoop.

-}

module Main where

import System.Console.CmdArgs (cmdArgs)

import Kurt.Config (kurtDefaultConfig)
import Kurt.MainLoop (startLoop)


main :: IO ()
main = do
  kurtConfig <- cmdArgs "kurt version 0.0.1, (C) Fabian Linzberger 2010" [kurtDefaultConfig]
  startLoop kurtConfig
