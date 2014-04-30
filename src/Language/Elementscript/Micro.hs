{-# LANGUAGE TypeHoles #-}
module Language.Elementscript.Micro (initialEvalState,
                                     EvalState(..),
                                     evaluate) where
import           Control.Applicative
import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Data.IntMap.Strict         (IntMap)
import qualified Data.IntMap.Strict         as IntMap
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Sequence              (Seq, ViewL (..), ViewR (..), viewl,
                                             viewr)
import qualified Data.Sequence              as Seq
import           Data.String                (IsString (..))
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as Text
import           Text.Parsec                hiding ((<|>))
import           Text.Parsec.Text.Lazy

data EvalState = ES {}

evaluate :: ParsecT Text EvalState IO ()
evaluate = do _

initialEvalState :: EvalState
initialEvalState = ES {}
