{-# LANGUAGE TemplateHaskell #-}

module Application.Types.Process
  ( Process(..)
  ) where

import Prelude hiding (id)

import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Text.Lazy (Text)

data Process = Process
  { id :: Int
  , user :: Text
  , host :: Text
  , db :: Maybe Text
  , command :: Text
  , time :: Int
  , state :: Maybe Text
  , info :: Text
  }

$(deriveToJSON defaultOptions ''Process)
