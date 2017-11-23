module Main where

import Control.Applicative
  ( (<**>)
  )
import System.IO
  ( BufferMode (LineBuffering)
  , hSetBuffering
  , stdout
  )

import Data.Semigroup
  ( (<>)
  )
import Options.Applicative
  ( Parser
  , argument
  , execParser
  , fullDesc
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , short
  , str
  , value
  )

import GFMake
  ( convertScript
  )

data CLI = CLI String String

cli :: Parser CLI
cli = CLI
  <$> argument str (metavar "INPUT" <> help "Input file")
  <*> option str (long "output" <> short 'o' <> value "" <> metavar "OUTPUT" <> help "Output file")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  main' =<< execParser opts
    where
      opts = info (cli <**> helper)
        (fullDesc <> progDesc "Convert game scripts to GameFAQs Markup.")

main' :: CLI -> IO ()
main' (CLI inputFile outputFile) = do
  content <- readFile inputFile
  let parsed = convertScript content
  if null outputFile
    then putStrLn parsed
    else writeFile outputFile parsed
