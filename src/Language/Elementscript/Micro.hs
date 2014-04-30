{-# LANGUAGE TypeHoles #-}
module Language.Elementscript.Micro (EvalState(..),
                                     initialEvalState,
                                     evaluate,
                                     Val(..),
                                     normalize,
                                     prettyPrint) where
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
import           Text.Parsec
import           Text.Parsec.Text.Lazy

data EvalState = ES {}

data Val :: * where
    Lam :: { lamBody :: Val -> Val } -> Val
    Var :: { varName :: Text } -> Val
    App :: { appFunc :: Val, appArg :: Val } -> Val

normalize :: Val -> Val
normalize (Lam body) = Lam $ normalize . body
normalize (Var name) = Var name
normalize (App (Lam body) arg) = normalize $ body arg
normalize (App (Var name) arg) = App (Var name) $ normalize arg

prettyPrint :: Val -> Text
prettyPrint = go _
    where
        go :: () -> Val -> Text
        go = _

evaluate :: ParsecT Text EvalState IO Val
evaluate = _

initialEvalState :: EvalState
initialEvalState = ES {}
