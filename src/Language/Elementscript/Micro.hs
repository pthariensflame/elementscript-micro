{-# LANGUAGE OverloadedStrings, GADTSyntax, KindSignatures #-}
module Language.Elementscript.Micro (initialEvalState,
                                     EvalState(..),
                                     evaluate,
                                     Val(..),
                                     EvalTree(..),
                                     -- * Text/Pipes Utilities
                                     stdinST,
                                     stdoutDT) where
import Data.Text (Text)
import qualified Data.Text as Text
import Data.String (IsString(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Language.Elementscript.Micro.Values

data Val :: * where
     Function :: (Val -> State EvalState Val) -> Val
     Lambda   ::                                           Val
     Declare  ::                                           Val
     Eval     ::                                           Val

data EvalTree :: * where
     Identifier   :: {                          identName :: Text                               } -> EvalTree
     FullApp      :: { leftSubtree :: EvalTree, opSubtree :: EvalTree, rightSubtree :: EvalTree } -> EvalTree
     LeftSection  :: { leftSubtree :: EvalTree, opSubtree :: EvalTree                           } -> EvalTree
     RightSection :: {                          opSubtree :: EvalTree, rightSubtree :: EvalTree } -> EvalTree

data EvalState = ES { valMap :: Map Text Val),
                      precedenceList :: IntMap Text }

initialEvalState :: EvalState
initialEvalState = ES { valMap = Map.fromList [ ("->", )
                                              , ("=" , )
                                              , ("in", )
                                              ]
                      , precedenceList = IntMap.fromList [ (0 , "->")
                                                         , (60, "=" )
                                                         , (50, "in")
                                                         ]
                      }

lookupId :: State EvalState

evaluate :: Seq Text -> State EvalState Seq Text
evaluate input state = 
