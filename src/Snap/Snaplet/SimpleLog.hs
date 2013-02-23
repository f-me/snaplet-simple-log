{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Snap.Snaplet.SimpleLog (
  SimpleLog(..),
  simpleLogInit, simpleLogInit_,
  
  module System.Log.Simple
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Snap.Core
import Snap.Snaplet
import System.Log.Simple

data SimpleLog = SimpleLog {
  simpleLog :: Log }

simpleLogInit :: RulesLoad -> [Logger] -> SnapletInit b SimpleLog
simpleLogInit rs ls = makeSnaplet "log" "Simple log" Nothing $ do
  l <- liftIO $ newLog rs ls
  return $ SimpleLog l

simpleLogInit_ :: Log -> SnapletInit b SimpleLog
simpleLogInit_ l = makeSnaplet "log" "Simple log" Nothing $ do
    return $ SimpleLog l

instance MonadLog (Handler b SimpleLog) where
  askLog = gets simpleLog
