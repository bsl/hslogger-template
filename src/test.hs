{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import System.Log.Logger.TH (deriveLoggers)
import qualified System.Log.Logger as HSL

$(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO, HSL.NOTICE, HSL.WARNING, HSL.ERROR, HSL.CRITICAL, HSL.ALERT, HSL.EMERGENCY])

main :: IO ()
main = do
    HSL.updateGlobalLogger "Main" (HSL.setLevel HSL.DEBUG)
    mapM_ (\(f, fn, i) -> f (show i ++ " " ++ fn)) (zip3 functions functionNames numbers)
  where
    functions     = [debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM]
    functionNames = ["debugM", "infoM", "noticeM", "warningM", "errorM", "criticalM", "alertM", "emergencyM"]
    numbers       = [1..] :: [Integer]
