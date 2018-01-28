module Main where

import Data.Semigroup ((<>))
import Options.Applicative

main :: IO ()
main = fetch =<< execParser opts'
  where
    opts' =
      info
        (opts <**> helper)
        (fullDesc <> progDesc "Fetch Netflix's My List for given regions" <>
         header "indb - The best way to find what to watch next")

fetch :: Opts -> IO ()
fetch (Opts rs _) = putStrLn $ "Fetching My List from regions: " ++ show rs

data Opts = Opts
  { regions :: String
  , dummy :: Bool
  }

opts :: Parser Opts
opts =
  Opts <$>
  strOption
    (long "regions" <> short 'R' <> metavar "REG[,REG]" <>
     help "Netflix regions you what to fetch My List from") <*>
  switch (long "dummy" <> short 'd' <> help "Not used")
