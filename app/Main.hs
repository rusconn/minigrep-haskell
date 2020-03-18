import RIO
import System.Environment

import App
import Lib

main :: IO ()
main = runApp $ do
  config <- liftIO getArgs >>= either argError id . newConfig
  run config `catchIO` appError

argError :: Utf8Builder -> App a
argError e = do
  logError $ "Problem parsing arguments: " <> e
  exitFailure

appError :: IOException -> App a
appError e = do
  logError $ "Application error: " <> display e
  exitFailure
