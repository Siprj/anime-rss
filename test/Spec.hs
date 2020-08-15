{-# LANGUAGE NoImplicitPrelude #-}

import System.IO (IO)
import Test.Tasty (TestTree, defaultMain, testGroup)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" []
