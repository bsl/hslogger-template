module Main (main) where

import System.Process (system)

import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, runTests)

main :: IO ()
main =
    defaultMainWithHooks $ simpleUserHooks { runTests = runTests' }
  where
    runTests' _ _ _ _ =
        mapM_ runTest ["test.hs", "testNamed.hs"]
      where
        runTest fp = do
            putStrLn $ fp ++ ":"
            system $ "runhaskell -i./src src/" ++ fp
            putStrLn ""
            return ()
