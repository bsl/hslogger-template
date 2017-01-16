{-# LANGUAGE TemplateHaskell, ExplicitForAll #-}

-- | This module provides functions that generate hslogger functions using
-- Template Haskell.
--
-- Notes:
--
-- * "System.Log.Logger" must be imported qualified, and the qualifier must
-- match the qualifier given to @deriveLoggers@ and/or @deriveNamedLoggers@.
--
-- * Don't forget to enable Template Haskell preprocessing: specify the pragma
-- @LANGUAGE TemplateHaskell@ at the top of your source file or
-- @extensions: TemplateHaskell@ in your cabal file.

module System.Log.Logger.TH
  ( deriveLoggers
  , deriveNamedLoggers
  )
  where

import Control.Monad.Trans (MonadIO, liftIO)
import qualified Language.Haskell.TH as TH

import qualified System.Log.Logger as HSL

-- | Generate hslogger functions for a list of priorities.
--
-- Example usage:
--
-- > module Foo.Bar ( ... ) where
-- >
-- > import System.Log.Logger.TH (deriveLoggers)
-- > import qualified System.Log.Logger as HSL
-- >
-- > $(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO])
--
-- Used this way, @deriveLoggers@ would generate the following functions:
--
-- > infoM :: MonadIO m => String -> m ()
-- > infoM s = liftIO (HSL.infoM "Foo.Bar" s)
-- >
-- > debugM :: MonadIO m => String -> m ()
-- > debugM s = liftIO (HSL.debugM "Foo.Bar" s)
--
-- The other hslogger priorities follow the same pattern.
--
-- /In versions prior to 2.0.0, hslogger-template generated functions that/
-- /prepended the module name to the log message. I no longer feel that this/
-- /is correct behavior. Instead, please make use of hslogger's formatting/
-- /functionality. Example:/
--
-- > import System.IO (stderr)
-- >
-- > import System.Log.Formatter      (simpleLogFormatter)
-- > import System.Log.Logger         (rootLoggerName)
-- > import System.Log.Handler        (setFormatter)
-- > import System.Log.Handler.Simple (streamHandler)
-- > import System.Log.Logger.TH      (deriveLoggers)
-- >
-- > import qualified System.Log.Logger as HSL
-- >
-- > $(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO])
-- >
-- > handler <- streamHandler stderr HSL.DEBUG >>= \h -> return $
-- >   setFormatter h $ simpleLogFormatter "$time $loggername $prio $msg"
-- > HSL.updateGlobalLogger rootLoggerName (HSL.setLevel HSL.DEBUG . HSL.setHandlers [handler])

deriveLoggers
  :: String          -- ^ Must match qualifier on import of "System.Log.Logger".
  -> [HSL.Priority]  -- ^ List of priorities for which to generate logging functions.
  -> TH.Q [TH.Dec]
deriveLoggers qualifier priorities =
    fmap TH.loc_module TH.location >>= \moduleName ->
      fmap concat (mapM (deriveLogger qualifier moduleName Nothing) priorities)

-- | Like @deriveLoggers@, but allows you to specify a message prefix to be
-- automatically prepended to every log message.

deriveNamedLoggers
  :: String          -- ^ Message prefix, e.g., "SomeProgram: ".
  -> String          -- ^ Must match qualifier on import of "System.Log.Logger".
  -> [HSL.Priority]  -- ^ List of priorities for which to generate logging functions.
  -> TH.Q [TH.Dec]
deriveNamedLoggers prefix qualifier priorities =
    fmap TH.loc_module TH.location >>= \moduleName ->
      fmap concat (mapM (deriveLogger qualifier moduleName (Just prefix)) priorities)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

deriveLogger :: String -> String -> Maybe String -> HSL.Priority -> TH.Q [TH.Dec]
deriveLogger qualifier moduleName mprefix priority = do
    sig  <- TH.sigD thF [t| forall m. MonadIO m => String -> m () |]
    body <- TH.funD thF
              [ TH.clause
                  [TH.varP thS]
                  (TH.normalB
                    (TH.appE
                      (TH.varE 'liftIO)
                      (TH.appE
                        -- HSL.xM "Foo.Bar"
                        (TH.appE
                          (TH.varE thH)
                          (TH.stringE moduleName)
                        )
                        (case mprefix of
                          Just prefix ->
                            -- (++) prefix s
                            TH.appE
                              (TH.appE
                                (TH.varE '(++))
                                (TH.stringE prefix)
                              )
                              (TH.varE thS)
                          Nothing ->
                            TH.varE thS
                        )
                      )
                    )
                  )
                  []
              ]
    return [sig, body]
  where
    thF = TH.mkName functionName
    thS = TH.mkName "s"
    thH = TH.mkName (qualifier ++ "." ++ functionName)
    functionName =
      case priority of
        HSL.DEBUG     -> "debugM"
        HSL.INFO      -> "infoM"
        HSL.NOTICE    -> "noticeM"
        HSL.WARNING   -> "warningM"
        HSL.ERROR     -> "errorM"
        HSL.CRITICAL  -> "criticalM"
        HSL.ALERT     -> "alertM"
        HSL.EMERGENCY -> "emergencyM"
