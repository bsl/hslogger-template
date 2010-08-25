{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import System.Log.Logger.TH (deriveNamedLoggers)
import qualified System.Log.Logger as HSL

$(deriveNamedLoggers "testNamed" "HSL" [HSL.DEBUG, HSL.INFO, HSL.NOTICE, HSL.WARNING, HSL.ERROR, HSL.CRITICAL, HSL.ALERT, HSL.EMERGENCY])

main :: IO ()
main = do
    HSL.updateGlobalLogger "testNamed" (HSL.setLevel HSL.DEBUG)
    mapM_ (\(f, fn, i) -> f (show i ++ " " ++ fn)) (zip3 functions functionNames numbers)
  where
    functions     = [debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM]
    functionNames = ["debugM", "infoM", "noticeM", "warningM", "errorM", "criticalM", "alertM", "emergencyM"]
    numbers       = [1..] :: [Integer]
