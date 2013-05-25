module Language.Elementscript.Micro.Values (Val(..),
                                            PrimVal(..),
                                            OpTree(..)) where
import Data.Text (Text)
import qualified Data.Text as Text
import Data.String (IsString(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Show
import Text.Read

data Val = Primitive PrimVal
         | Element (Map PrimVal Val)
         | Function (Val -> IO Val)
         | TreeMacro (OpTree -> IO Val)
         | TextMacro (Text -> IO Val)

data PrimVal = PrimInt Integer
             | PrimText Text deriving (Eq, Ord)

instance Show PrimVal where
  showsPrec d (PrimInt i) = showsPrec d i
  showsPrec d (PrimText t) = showsPrec d $ Text.unpack t

instance Read PrimVal where
  readPrec = fmap PrimInt readPrec +++ fmap (PrimText . Text.pack) readPrec
  readListPrec = readListPrecDefault

instance IsString PrimVal where
  fromString = PrimText . Text.pack

data OpTree = Variable { varName :: Text }
            | PrimValue { getPrimValue :: PrimVal }
            | FullApp { leftSubtree :: OpTree, 
                        opSubtree :: OpTree,
                        rightSubtree :: OpTree }
            | LeftSection { leftSubtree :: OpTree, 
                            opSubtree :: OpTree }
            | RightSection { opSubtree :: OpTree,
                             rightSubtree :: OpTree } deriving (Eq, Read, Show)
