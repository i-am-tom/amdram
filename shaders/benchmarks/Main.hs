{-# LANGUAGE BlockArguments #-}

module Main where

import Criterion.Main (bench, bgroup, defaultConfig, defaultMainWith, nfAppIO)
import Criterion.Types (jsonFile)

main :: IO ()
main =
  defaultMainWith
    defaultConfig {jsonFile = Just "criterion.json"}
    [ bgroup "Placeholder Benchmark" do
        count <- [floor @Float (10 ** x) | x <- [1 .. 7]]

        pure $ bench (show count) do
          nfAppIO example count
    ]

example :: Int -> IO ()
example count = sum [1 .. count] `seq` pure ()
