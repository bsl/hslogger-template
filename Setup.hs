module Main (main) where

import System.Cmd (system)

import Distribution.Simple (defaultMainWithHooks, runTests, simpleUserHooks)

main :: IO ()
main =
    defaultMainWithHooks $
      simpleUserHooks
        { runTests = \_ _ _ _ ->
            system "runhaskell -i./src -Wall -XTemplateHaskell src/test.hs" >> return ()
        }
