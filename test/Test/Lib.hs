module Test.Lib where

import RIO
import Test.Tasty.HUnit
import Lib

unit_caseSensitive :: Assertion
unit_caseSensitive =
  let query'   = "duct"
      contents = "Haskell:\nsafe, pure, productive.\nPick three.\nDuct tape."
      expected = ["safe, pure, productive."]
   in expected @=? search query' contents True

unit_caseInsensitive :: Assertion
unit_caseInsensitive =
  let query'   = "hAsK"
      contents = "Haskell:\nsafe, pure, productive.\nPick three.\nHaskell B. Curry."
      expected = ["Haskell:", "Haskell B. Curry."]
   in expected @=? search query' contents False
