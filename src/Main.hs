module Main (main) where
import           Data.Text.Lazy
import           Data.Text.Lazy.IO
import           Language.Elementscript.Micro
import           Prelude                      hiding (getContents, putStrLn)
import           System.IO                    (stderr)
import           Text.Parsec
import           Text.Parsec.Text.Lazy        ()

main :: IO ()
main = getContents >>= runParserT evaluate initialEvalState "" >>= \case
    Left err -> hPutStrLn stderr . pack $ show err
    Right val -> putStrLn $ prettyPrint $ normalize val
