{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Parser (parse)
import Semantic (typecheck)

test :: [String] -> IO ()
test [file] = do
  source <- readFile file
  case parse source of
    Left err -> putStrLn ("Error when parsing: " ++ err)
    Right prog -> print $ typecheck prog
test _ = putStrLn "usage: compiler source_file"

main :: IO ()
main = do
  args <- getArgs
  test args


