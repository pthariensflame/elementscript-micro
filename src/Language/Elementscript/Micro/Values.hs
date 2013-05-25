module Language.Elementscript.Micro.Values (Val(..),
                                            PrimVal(..),
                                            OpTree(..)) where

data Val = Primitive PrimVal
         | Element (Map PrimVal Val)
         | Function (Val -> IO Val)
         | TreeMacro (OpTree -> IO Val)
         | TextMacro (Text -> IO Val)

data PrimVal = PrimInt Integer
             | PrimText Text deriving (Eq, Ord)

instance Show PrimVal where
  showsPrec

data OpTree = 
