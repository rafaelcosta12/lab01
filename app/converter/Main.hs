module Main where

import AutomatonTypes (DFA)
import ParseYAML (readYAMLFile, AutomatoDef (autType), toNFAE, toNFA, toDFA, automatoDefToFile, dfaToAutomatoDef)
import Data.Text (unpack)
import Operations (nfaeParaNfa, nfaParaDfa, mostrarNFA, mostrarDFA, aceitarDFA)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> run filePath Nothing
        [filePath, testStr] -> run filePath (Just testStr)
        _ -> error "Usage: converter <path_to_yaml_file> [<test_string>]"

run :: FilePath -> Maybe String -> IO ()
run filePath mTestStr = do
    automatoDef <- readYAMLFile filePath
    case unpack (autType automatoDef) of
        "nfa" -> runNfa automatoDef mTestStr
        "nfae" -> runNfae automatoDef mTestStr
        "dfa" -> runDfa automatoDef mTestStr
        _ -> putStrLn "Tipo de autômato desconhecido!"

runNfae :: AutomatoDef -> Maybe String -> IO ()
runNfae automatoDef mTestStr = do
    let nfae = toNFAE automatoDef
    mostrarNFA nfae
    let nfa = nfaeParaNfa nfae
    print "----- Conversão de NFAE para NFA concluida! -----"
    mostrarNFA nfa
    let dfa = nfaParaDfa nfa
    print "----- Conversão de NFA para DFA concluida! -----"
    mostrarDFA dfa
    automatoDefToFile (dfaToAutomatoDef dfa) "dfa_converted.yaml"
    maybeTest dfa mTestStr

runNfa :: AutomatoDef -> Maybe String -> IO ()
runNfa automatoDef mTestStr = do
    let nfa = toNFA automatoDef
    mostrarNFA nfa
    let dfa = nfaParaDfa nfa
    print "----- Conversão de NFA para DFA concluida! -----"
    mostrarDFA dfa
    automatoDefToFile (dfaToAutomatoDef dfa) "dfa_converted.yaml"
    maybeTest dfa mTestStr

runDfa :: AutomatoDef -> Maybe String -> IO ()
runDfa automatoDef mTestStr = do
    let dfa = toDFA automatoDef
    mostrarDFA dfa
    maybeTest dfa mTestStr

maybeTest :: DFA -> Maybe String -> IO ()
maybeTest dfa (Just testStr) = do
    let symbols = [[c] | c <- testStr]
    let result = aceitarDFA dfa symbols
    putStrLn $ "String: \"" ++ testStr ++ "\""
    putStrLn $ "Resultado: " ++ if result then "ACEITA" else "REJEITA"
maybeTest _ Nothing = return ()
