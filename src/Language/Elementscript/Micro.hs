{-# LANGUAGE TypeHoles         #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Language.Elementscript.Micro (initialEvalState,
                                     EvalState(..),
                                     evaluate) where
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
import           Text.Parsec

data EvalState = ES {}

evaluate :: [Text] -> State EvalState [Text]
evaluate = _ . breakIntoDecls

initialEvalState :: EvalState
initialEvalState = _

breakIntoDecls :: [Text] -> ([Decl], Text)
breakIntoDecls input = _

data Decl = Decl { declName :: Text, declVars :: Seq Text, declDef :: Text }
