{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.DeepSeq (($!!), deepseq, force, NFData, rnf)
import           Control.Monad
import           Data.Aeson
import           HS2AST.Types

data ASTId = ASTId { aId   :: !Identifier
                   , aDeps :: ![Identifier]
                   } deriving (Show, Eq)

instance NFData ASTId where
  rnf (ASTId id deps) = id `deepseq` deps `deepseq` ()

instance NFData Identifier where
  rnf i = idName i `deepseq` idPackage i `deepseq` idModule i `deepseq` ()

aName    = idName    . aId
aPackage = idPackage . aId
aModule  = idModule  . aId

instance ToJSON ASTId where
  toJSON x = let i = aId x
              in object [
                     "name"         .= idName    i
                   , "package"      .= idPackage i
                   , "module"       .= idModule  i
                   , "dependencies" .= aDeps     x
                   ]

instance FromJSON ASTId where
  parseJSON (Object x) = do
    n <- x .: "name"
    p <- x .: "package"
    m <- x .: "module"
    d <- x .: "dependencies"
    return $!! ASTId {
        aId = ID {
            idName    = n
          , idPackage = p
          , idModule  = m
          }
      , aDeps = d
      }
  parseJSON _ = mzero
