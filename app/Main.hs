import RIO
import System.Environment (getArgs)

import App
import Lib

main :: IO ()
main = runApp $ do
  args <- liftIO getArgs

  config <- either
    (\action -> do
      msg <- action
      logError $ "Problem parsing arguments: " <> msg
      exitFailure)
    id
    (newConfig args)

  run config `catchIO` \e -> do
    logError $ "Application error: " <> display e
    exitFailure
