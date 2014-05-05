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
import           Data.Monoid                (Monoid (..), (<>))
import           Data.Sequence              (Seq, ViewL (..), ViewR (..), viewl,
                                             viewr)
import qualified Data.Sequence              as Seq
import           Data.String                (IsString (..))
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as Text
import           Text.Parsec
import           Text.Parsec.Text.Lazy

data EvalState = ES { nameMap :: Map Text Val,
                      precMap :: IntMap Text }

data Val = Lam { lamVar  :: Text,
                 lamBody :: Val }
         | Var { varName :: Text }
         | App { appFunc :: Val,
                 appArg  :: Val }

normalize :: Val -> Val
normalize (Lam var body) = Lam var $ normalize body
normalize (Var name) = Var name
normalize (App (Lam var body) arg) = normalize $ subst var arg body
normalize (App (Var name) arg) = App (Var name) $ normalize arg

subst :: Text -> Val -> Val -> Val
subst outerVar expr (Lam innerVar body) | outerVar == innerVar = Lam innerVar body
                                        | otherwise = Lam innerVar (subst outerVar expr body)
subst var expr (Var name) | var == name = expr
                          | otherwise = Var name
subst var expr (App func arg) = App (subst var expr func) (subst var expr arg)

prettyPrint :: Val -> Text
prettyPrint = go False
    where
        go :: Bool -> Val -> Text
        go underApp (Lam var body) = ppParens underApp (var <> " -> " <> go False body)
        go _ (Var name) = name
        go underApp (App func arg) = ppParens underApp (go True func <> " " <> go True arg)

        ppParens :: Bool -> Text -> Text
        ppParens wanted inner = if wanted then "(" <> inner <> ")" else inner

evaluate :: ParsecT Text EvalState IO Val
evaluate = _

initialEvalStateList :: [(Text, Int, Val)]
initialEvalStateList = _

initialEvalState :: EvalState
initialEvalState = ES { nameMap = Map.fromList $ map (\(name, _, val) -> (name, val)) initialEvalStateList,
                        precMap = IntMap.fromList $ map (\(name, prec, _) -> (prec, name)) initialEvalStateList }
