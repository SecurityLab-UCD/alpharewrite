{-# LANGUAGE OverloadedStrings #-}

module Main where

import AlphaRewrite
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
             []      -> BL.getContents
             [file]  -> BL.readFile file
             _       -> error "Usage: alpha-rename [tasks.json]"

  -- We expect an array of tasks in the input JSON
  case decode input :: Maybe [Task] of
    Nothing -> error "Could not parse JSON."
    Just tasks -> do
      let renamed = map alphaRewriteTask tasks
      BL.putStr (encode renamed)
