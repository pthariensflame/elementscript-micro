{-# LANGUAGE TypeHoles #-}
module Language.Elementscript.Micro (initialEvalState,
                                     EvalState(..),
                                     evaluate) where
import Control.Monad.State
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.String (IsString(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data AST :: * where
    Value :: AST

data EvalState = ES {}

initialEvalState :: EvalState
initialEvalState = _

evaluate :: Seq Text -> State EvalState (Seq Text)
evaluate input = _
