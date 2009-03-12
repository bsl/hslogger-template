{-# LANGUAGE TemplateHaskell #-}

-- | This module provides a Template Haskell function for generating hslogger
-- functions automatically.
module System.Log.TH (deriveLoggers) where

import Control.Monad (liftM)
import qualified Language.Haskell.TH as TH

import qualified System.Log.Logger as HSL

-- | Generate hslogger functions.
--
-- Example usage:
--
-- > module Foo.Bar ( ... ) where
-- >
-- > import System.Log.TH (deriveLoggers)
-- > import qualified System.Log.Logger as HSL
-- >
-- > $(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO])
--
-- Used this way, @deriveLoggers@ would generate the following functions:
--
-- > info :: String -> IO ()
-- > info s = HSL.infoM "Foo.Bar" ((++) "Foo.Bar: " s)
-- >
-- > debug :: String -> IO ()
-- > debug s = HSL.debugM "Foo.Bar" ((++) "Foo.Bar: " s)
--
-- The other hslogger 'Priority's follow the same pattern.
--
-- So
--
-- > info "hi there"
--
-- would produce the INFO-level log entry
--
-- > Foo.Bar: hi there
--
-- Note: "System.Log.Logger" must be imported qualified, and the qualifier must
-- match the qualifier given to @deriveLoggers@.
--
deriveLoggers :: String          -- ^ Must match qualifier on import of "System.Log.Logger".
              -> [HSL.Priority]  -- ^ List of priorities for which to generate logging functions.
              -> TH.Q [TH.Dec]
deriveLoggers qualifier priorities =
  liftM TH.loc_module TH.location >>= \moduleName ->
  liftM concat (mapM (deriveLogger qualifier moduleName) priorities)

-- ----------------------------------------

deriveLogger :: String -> String -> HSL.Priority -> TH.Q [TH.Dec]
deriveLogger qualifier moduleName priority = code where
  code = do
    sig  <- TH.sigD th_f [t| String -> IO () |]
    body <- TH.funD th_f
              [ TH.clause [TH.varP th_s]
                (TH.normalB
                  (TH.appE
                    (TH.appE
                      (TH.varE th_h)
                      (TH.stringE moduleName)
                    )
                    (TH.appE
                      (TH.appE
                        (TH.varE '(++))
                        (TH.litE (TH.stringL (moduleName ++ ": ")))
                      )
                      (TH.varE th_s)
                    )
                  )
                )
                []
              ]
    return [sig, body]
    where
      th_f = TH.mkName functionName
      th_h = TH.mkName $ concat [qualifier, ".", functionName, "M"]
      th_s = TH.mkName "s"
      functionName =
        case priority of
          HSL.DEBUG     -> "debug"
          HSL.INFO      -> "info"
          HSL.NOTICE    -> "notice"
          HSL.WARNING   -> "warning"
          HSL.ERROR     -> "error"
          HSL.CRITICAL  -> "critical"
          HSL.ALERT     -> "alert"
          HSL.EMERGENCY -> "emergency"
