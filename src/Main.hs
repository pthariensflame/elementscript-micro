module Main (main) where
import Prelude hiding (interact, words, unwords)
import Data.Text.Lazy
import Data.Text.Lazy.IO
import qualified Data.Sequence as Seq
import Control.Monad.State
import Data.Foldable (fold, toList)
import Language.Elementscript.Micro

main :: IO ()
main = interact $ \input -> unwords . toList . flip evalState initialEvalState . evaluate . Seq.fromList $ words input
