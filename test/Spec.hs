{-# LANGUAGE NoImplicitPrelude #-}

import System.IO (IO)
import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.DataModel.Type.TH as TH (tests)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ TH.tests
    ]
