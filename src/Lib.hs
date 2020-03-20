module Lib
  ( Config (..)
  , newConfig
  , run
  , search
  ) where

import           RIO
import           RIO.Process
import qualified RIO.Text         as T
import qualified RIO.Vector.Boxed as VB

import           App

data Config = Config
  { query :: Text
  , filename :: FilePath
  , caseSensitive :: Bool
  }

newConfig :: [String] -> Either (App Utf8Builder) (App Config)
newConfig []  = Left $ pure "Didn't get a query string"
newConfig [_] = Left $ pure "Didn't get a file name"
newConfig (query:filename:_) = Right $
  Config (T.pack query) filename . isNothing
    <$> lookupEnvFromContext "CASE_INSENSITIVE"

run :: Config -> App ()
run Config {..} = do
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
