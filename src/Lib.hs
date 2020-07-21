module Lib where

import           RIO
import           RIO.Process
import qualified RIO.Text    as T

import           Conduit

import           App

data Config = Config
  { query :: Text
  , filename :: FilePath
  , caseSensitive :: Bool
  }

newConfig :: [String] -> Either Utf8Builder (App Config)
newConfig = \case
  []  -> Left "Didn't get a query string"
  [_] -> Left "Didn't get a file name"
  (query:filename:_) -> Right $ fmap
    do Config (T.pack query) filename . isNothing
    do lookupEnvFromContext "CASE_INSENSITIVE"

run :: Config -> RIO SimpleApp ()
run Config {..} =
  let lowerQuery = T.toLower query
      predicate  = if caseSensitive
        then T.isInfixOf query
        else T.isInfixOf lowerQuery . T.toLower
   in runConduitRes
         $ sourceFile filename
        .| decodeUtf8C
        .| linesUnboundedC
        .| filterC predicate
        .| unlinesC
        .| encodeUtf8C
        .| stdoutC
