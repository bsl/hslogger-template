{-# LANGUAGE TemplateHaskell #-}

-- | This module provides functions that generate hslogger functions using
--   Template Haskell.
module System.Log.Logger.TH
  (
    deriveLoggers,
    deriveNamedLoggers
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
-- > infoM s = liftIO (HSL.infoM "Foo.Bar" ((++) "Foo.Bar: " s))
-- >
-- > debugM :: MonadIO m => String -> m ()
-- > debugM s = liftIO (HSL.debugM "Foo.Bar" ((++) "Foo.Bar: " s))
--
-- The other hslogger priorities follow the same pattern.
--
-- So
--
-- > infoM "hi there"
--
-- would generate the INFO-level log event
--
-- > Foo.Bar: hi there
--
-- Notes:
--
--   * "System.Log.Logger" must be imported qualified, and the qualifier must
--   match the qualifier given to @deriveLoggers@.
--
--   * Don't forget to enable Template Haskell preprocessing: specify the
--   pragma @LANGUAGE TemplateHaskell@ at the top of your source file or
--   @extensions: TemplateHaskell@ in your cabal file.


deriveNamedLoggers
  :: String          -- ^ Name for the logger to derive.
  -> String          -- ^ Must match qualifier on import of "System.Log.Logger".
  -> [HSL.Priority]  -- ^ List of priorities for which to generate logging functions.
  -> TH.Q [TH.Dec]
deriveNamedLoggers moduleName qualifier priorities =
      fmap concat (mapM (deriveLogger qualifier moduleName) priorities)


deriveLoggers
  :: String          -- ^ Must match qualifier on import of "System.Log.Logger".
  -> [HSL.Priority]  -- ^ List of priorities for which to generate logging functions.
  -> TH.Q [TH.Dec]
deriveLoggers qualifier priorities =
    fmap TH.loc_module TH.location >>= \moduleName ->
      fmap concat (mapM (deriveLogger qualifier moduleName) priorities)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

deriveLogger :: String -> String -> HSL.Priority -> TH.Q [TH.Dec]
deriveLogger qualifier moduleName priority = do
    sig  <- TH.sigD thF [t| MonadIO m => String -> m () |]
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
                        -- ((++) "Foo.Bar: ") s
                        (TH.appE
                          (TH.appE
                            (TH.varE '(++))
                            (TH.stringE prefix)
                          )
                          (TH.varE thS)
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
    prefix = moduleName ++ ": "
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
