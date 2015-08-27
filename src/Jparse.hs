{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Jparse where

import           Data.Aeson
import           Data.Text
import           GHC.Generics



data ASTId =
  ASTId {  name         :: Text
         , modu         :: Text
         , package      :: Text
         , dependencies :: Maybe [ASTId]
        } deriving (Show, Generic, Eq)

instance FromJSON ASTId
instance ToJSON ASTId
