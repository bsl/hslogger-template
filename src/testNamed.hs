{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import System.IO (stderr)

import System.Log.Formatter      (simpleLogFormatter)
import System.Log.Handler        (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger         (rootLoggerName)
import System.Log.Logger.TH      (deriveNamedLoggers)

import qualified System.Log.Logger as HSL

$(deriveNamedLoggers "SomeProgram: " "HSL" [HSL.DEBUG, HSL.INFO, HSL.NOTICE, HSL.WARNING, HSL.ERROR, HSL.CRITICAL, HSL.ALERT, HSL.EMERGENCY])

main :: IO ()
main = do
    handler <- streamHandler stderr HSL.DEBUG >>= \h -> return $
      setFormatter h $ simpleLogFormatter "$loggername $prio $msg"
    HSL.updateGlobalLogger rootLoggerName (HSL.setLevel HSL.DEBUG . HSL.setHandlers [handler])
    mapM_ (\(f, fn) -> f fn) (zip functions functionNames)
  where
    functions     = [debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM]
    functionNames = ["debugM", "infoM", "noticeM", "warningM", "errorM", "criticalM", "alertM", "emergencyM"]
