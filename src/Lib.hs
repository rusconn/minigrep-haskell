module Lib
  ( Config (..)
  , AppException
  , newConfig
  , run
  , search
  ) where

import           RIO
import           RIO.Process
import qualified RIO.Text         as T
import qualified RIO.Vector.Boxed as VB

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
run Config{..} = do
  contents <- readFileUtf8 filename
  let results = search query contents caseSensitive
  mapM_ (logInfo . display) results

search :: Text -> Text -> Bool -> Vector Text
search query contents caseSensitive =
  let lowerQuery = T.toLower query
      predicate  = if caseSensitive
        then T.isInfixOf query
        else T.isInfixOf lowerQuery . T.toLower
   in VB.filter predicate . VB.fromList $ T.lines contents
