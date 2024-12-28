{-# LANGUAGE DeriveGeneric     #-}

module Task (Task(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)


data Task = Task
  { task_id      :: Text
  , poly_type    :: Text
  , signature    :: Text
  , code         :: Text
  , dependencies :: [Text]
  } deriving (Show, Generic, Eq)

instance FromJSON Task
instance ToJSON   Task