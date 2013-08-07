module Main (main) where
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Proxy
import Control.Proxy.Trans.State
import Language.Elementsong.Micro

main :: IO ()
main = runProxy $ stdinST >-> evalStateK initialEvalState execute
