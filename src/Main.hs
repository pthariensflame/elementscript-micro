module Main (main) where
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as T
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Control.Proxy
import Language.Elementsong.Micro

main :: IO ()
main = runProxy $ stdinST >-> tokenize >-> recognize >-> evaluate 
