module Language.Elementscript.Micro (initialEvalState,
                                     EvalState(..),
                                     evaluate,
                                     Val(..),) where
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as Text
import Data.String (IsString(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Val :: * where
     Function :: (Val -> State EvalState Val) -> Val
     Lambda   ::                                 Val
     Declare  ::                                 Val
     In       ::                                 Val
     WithPrec ::                                 Val

data EvalState = ES { valMap :: Map Text Val,
                      precedenceMap :: IntMap Text }

initialEvalState :: EvalState
initialEvalState = ES { valMap = Map.fromList [ ("->"  , Lambda  )
                                              , ("="   , Declare )
                                              , ("in"  , In      )
                                              , ("prec", WithPrec)
                                              ]
                      , precedenceMap = IntMap.fromList [ (0  , "->"  )
                                                        , (100, "="   )
                                                        , (300, "in"  )
                                                        , (200, "prec")
                                                        ]
                      }

lookupId :: Text -> State EvalState Val
lookupId = undefined

evaluate :: Seq Text -> State EvalState (Seq Text)
evaluate input = do ES vM pM <- get
                    undefined
