module Main (main) where
import Prelude hiding (interact)
import Data.Text.Lazy.IO (interact)
import Control.Monad.State
import Language.Elementsong.Micro

main :: IO ()
main = interact $ evalState
