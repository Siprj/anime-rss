module Lib
  ( run
  )
where

import Relude

run :: IO ()
run = print @String "hello world"
