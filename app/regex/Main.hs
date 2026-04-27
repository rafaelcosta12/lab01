module Main where

import RegularExpressions (parse, build)
import Operations (mostrarNFA)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [regexStr] -> do
            let reg = parse regexStr
            putStrLn $ "Regex: " ++ regexStr
            putStrLn $ "Árvore sintática: " ++ show reg
            let nfa = build reg
            mostrarNFA nfa
        _ -> error "Usage: regex <regex_pattern>"
