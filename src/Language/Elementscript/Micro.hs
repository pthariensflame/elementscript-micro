{-# LANGUAGE OverloadedStrings #-}
module Language.Elementscript.Micro (tokenize,
                                     recognize,
                                     evaluate,
                                     Token(..),
                                     -- * Text/Pipes Utilities
                                     stdinST,
                                     stdoutDT) where
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.String (IsString(..))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Control.Proxy

tokenize :: (Proxy p, Monad m) => () -> Pipe Text [[Text]] m r
tokenize () = mapD

stdinST :: (Proxy p) => () -> Producer p Text IO r
stdinST () = runIdentityP . forever $ do str <- lift T.getLine
                                         respond str
{-# INLINABLE stdinS #-}

stdoutDT :: (Proxy p) => x -> p x String x String IO r
stdoutDT = runIdentityK . foreverK $ \x -> do a <- request x
                                              lift $ putStrLn a
                                              respond a
{-# INLINABLE stdoutD #-}

