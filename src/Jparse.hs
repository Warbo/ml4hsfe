{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Jparse where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Graph           as G
import qualified Data.Map             as Map
import           Data.Text
import           GHC.Generics



data ASTId =
  ASTId {  name         :: Text
         , modu         :: Text
         , package      :: Text
         , dependencies :: Maybe [ASTId]
        } deriving (Show, Generic)

instance FromJSON ASTId
instance ToJSON ASTId
