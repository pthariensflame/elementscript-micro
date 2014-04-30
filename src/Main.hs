{-# LANGUAGE TypeHoles #-}
module Main (main) where
import           Data.Text.Lazy               hiding (map)
import           Data.Text.Lazy.IO
import           Language.Elementscript.Micro
import           Prelude                      hiding (interact, unwords, words)
import           Text.Parsec

main :: IO ()
main = interact $ \input -> unwords . runParser _ _ "stdin" $ words input
