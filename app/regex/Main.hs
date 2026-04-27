module Main where

import RegexParser (parse)
import RegexTypes (build)
import Display (mostrarNFA)
import Conversion (nfaParaDfa)
import Acceptance (aceitarDFA)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [regexStr] ->
            showNFAS regexStr

        [regexStr, testStr] -> do
            showNFAS regexStr
            let nfa = build (parse regexStr)
            let dfa = nfaParaDfa nfa
            let symbols = [[c] | c <- testStr]
            let result = aceitarDFA dfa symbols
            putStrLn $ "String: \"" ++ testStr ++ "\""
            putStrLn $ "Resultado: " ++ if result then "ACEITA" else "REJEITA"

        _ -> error "Usage: regex <regex_pattern> [<test_string>]"

showNFAS :: String -> IO ()
showNFAS regexStr = do
    let reg = parse regexStr
    let nfa = build reg
    putStrLn $ "Regex: " ++ regexStr
    putStrLn $ "Arvore sintatica: " ++ show reg
    mostrarNFA nfa
