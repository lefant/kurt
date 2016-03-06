-- This is the actual entry point for running the program. Command line
-- option processing happens here, then Kurt.MainLoop.loop is launched
-- via Kurt.MainLoop.startLoop.

module Main where

import           System.Console.CmdArgs (cmdArgs)

import           Kurt.Config            (kurtDefaultConfig)
import           Kurt.MainLoop          (startLoop)


main :: IO ()
main = do
  -- "kurt version 0.0.4, (C) Fabian Linzberger 2010-2016" [kurtDefaultConfig]
  kurtConfig <- cmdArgs kurtDefaultConfig
  startLoop kurtConfig
