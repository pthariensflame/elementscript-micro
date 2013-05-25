{-# LANGUAGE OverloadedStrings #-}
module Language.Elementscript.Micro (-- * Simple Interface
                                     initialEvalState,
                                     execute,
                                     -- * Complex Interface
                                     module Language.Elementscript.Micro.Values,
                                     EvalState(..),
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
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Proxy
import Control.Proxy.Trans.State
import Language.Elementscript.Micro.Values

data EvalState = ES { valMap :: Map Text (EvalState -> IO (Val, EvalState)),
                      precedenceList :: IntMap Text }

initialEvalState :: EvalState
initialEvalState = ES { varMap = Map.fromList [("(", ),
                                               (")", ),
                                               ("[", ),
                                               ("]", ),
                                               ("{", ),
                                               ("}", ),
                                               (",", ),
                                               (":", ),
                                               ("eval", ),
                                               ("proc", ),
                                               ("macro", ),
                                               ("tmacro", ),
                                               ("list", ),,
                                               ("app", ),,
                                               ("apply", )],
                        precedenceList = IntMap.fromList [(0, "("),
                                                          (1, ")"),
                                                          (, "["),
                                                          (, "]"),
                                                          (, "{"),
                                                          (, "}"),
                                                          (, ","),
                                                          (, ":"),
                                                          (, "eval"),
                                                          (, "proc"),
                                                          (, "macro"),
                                                          (, "tmacro"),
                                                          (, "list"),
                                                          (, "app"),
                                                          (, "apply")]}

execute :: (Proxy p) => () -> Consumer (StateP EvalState p) Text IO ()
execute () = do 

stdinST :: (Proxy p) => () -> Producer p Text IO r
stdinST () = runIdentityP . forever $ do str <- lift Text.getLine
                                         respond str
{-# INLINABLE stdinST #-}

stdoutDT :: (Proxy p) => x -> p x String x String IO r
stdoutDT = runIdentityK . foreverK $ \x -> do a <- request x
                                              lift $ Text.putStrLn a
                                              respond a
{-# INLINABLE stdoutDT #-}

