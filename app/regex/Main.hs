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
        [regexStr] -> do
            let reg = parse regexStr
            putStrLn $ "Regex: " ++ regexStr
            putStrLn $ "Árvore sintática: " ++ show reg
            let nfa = build reg
            mostrarNFA nfa
        [regexStr, testStr] -> do
            let reg = parse regexStr
            let nfa = build reg
            let dfa = nfaParaDfa nfa
            let symbols = [[c] | c <- testStr]
            let result = aceitarDFA dfa symbols
            mostrarNFA nfa
            putStrLn $ "String: " ++ testStr
            putStrLn $ "Resultado: " ++ if result then "ACEITA" else "REJEITA"
        _ -> error "Usage: regex <regex_pattern> [<test_string>]"
