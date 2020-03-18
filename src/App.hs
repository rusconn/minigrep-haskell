module App where

import RIO

type App = RIO SimpleApp

runApp :: App a -> IO a
runApp = runSimpleApp
