module Lib
  ( Config (..)
  , AppException
  , newConfig
  , run
  ) where

import           RIO
import           RIO.Process
import qualified RIO.Text    as T

import           Conduit

data Config = Config
  { query :: Text
  , filename :: FilePath
  , caseSensitive :: Bool
  }

data AppException
  = QueryNotFound
  | FilenameNotFound
  deriving Show

instance Exception AppException

instance Display AppException where
  display QueryNotFound    = "Didn't get a query string"
  display FilenameNotFound = "Didn't get a file name"

newConfig :: [String] -> RIO SimpleApp Config
newConfig []  = throwIO QueryNotFound
newConfig [_] = throwIO FilenameNotFound
newConfig (query:filename:_) =
  Config (T.pack query) filename . isNothing
    <$> lookupEnvFromContext "CASE_INSENSITIVE"

run :: Config -> RIO SimpleApp ()
run Config{..} =
  let lowerQuery = T.toLower query
      predicate  = if caseSensitive
        then T.isInfixOf query
        else T.isInfixOf lowerQuery . T.toLower
  in  runConduitRes
        $  sourceFile filename
        .| decodeUtf8C
        .| linesUnboundedC
        .| filterC predicate
        .| unlinesC
        .| encodeUtf8C
        .| stdoutC
