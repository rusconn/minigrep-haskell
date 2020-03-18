import RIO
import System.Environment (getArgs)

import Lib

main :: IO ()
main = runSimpleApp $ do
  args <- liftIO getArgs

  config <- newConfig args `catchAny` \e -> do
    logError $ "Problem parsing arguments: " <> display e
    exitFailure

  run config `catchIO` \e -> do
    logError $ "Application error: " <> display e
    exitFailure
