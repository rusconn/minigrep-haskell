module App where

import RIO

type App a = RIO SimpleApp a

runApp :: RIO SimpleApp a -> IO a
runApp = runSimpleApp
