{-# LANGUAGE OverloadedStrings #-}
module Language.Elementscript.Micro (tokenize,
                                     EvalState(..),
                                     initialEvalState,
                                     evaluate,
                                     -- * Text/Pipes Utilities
                                     stdinST,
                                     stdoutDT) where
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.String (IsString(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (Map)
import qualified Data.Map.Strict as IntMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Proxy
import Control.Proxy.Trans.State

tokenize :: Text -> Seq Text
tokenize = S.fromList . T.words
{-# INLINABLE tokenize #-}

data EvalState p = ES { varMap :: Map Text (Seq Text -> () -> ), 
                        precedenceList :: IntMap (Set Text) }

initialEvalState :: EvalState
initialEvalState = ES {  }

evaluate :: (Proxy p) => () -> Consumer (StateP (EvalState p) p) (Seq Text) IO ()

stdinST :: (Proxy p) => () -> Producer p Text IO r
stdinST () = runIdentityP . forever $ do str <- lift Text.getLine
                                         respond str
{-# INLINABLE stdinST #-}

stdoutDT :: (Proxy p) => x -> p x String x String IO r
stdoutDT = runIdentityK . foreverK $ \x -> do a <- request x
                                              lift $ Text.putStrLn a
                                              respond a
{-# INLINABLE stdoutDT #-}

