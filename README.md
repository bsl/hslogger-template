`hslogger-template` generates [`hslogger`][1] functions using
[Template Haskell][2]. The generated functions specify the name of the current module, so
you don't have to.

Example:

    module Foo.Bar ( ... ) where

    import System.Log.Logger.TH (deriveLoggers)
    import qualified System.Log.Logger as HSL

    $(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO])

Used this way, `deriveLoggers` would generate the following functions:

    infoM :: MonadIO m => String -> m ()
    infoM s = liftIO (HSL.infoM "Foo.Bar" s)

    debugM :: MonadIO m => String -> m ()
    debugM s = liftIO (HSL.debugM "Foo.Bar" s)

The other hslogger priorities follow the same pattern.

[1]: http://hackage.haskell.org/package/hslogger
[2]: http://www.haskell.org/haskellwiki/Template_Haskell
