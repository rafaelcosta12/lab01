module Main where

import ParseYAML (readYAMLFile, AutomatoDef (autType), toNFAE, toNFA, automatoDefToFile, nfaToAutomatoDef, dfaToAutomatoDef)
import Data.Text (unpack)
import Operations (nfaeParaNfa, nfaParaDfa, mostrarNFA, mostrarDFA)
import System.Environment ( getArgs )

getFilePath :: IO FilePath
getFilePath = do
    args <- getArgs
    case args of
        [filePath] -> return filePath
        _          -> error "Usage: runhaskell Main.hs <path_to_yaml_file>"

main :: IO ()
main = do
    filePath <- getFilePath
    automatoDef <- readYAMLFile filePath
    case unpack (autType automatoDef) of
        "nfa" -> runNfa automatoDef
        "nfae" -> runNfae automatoDef
        "dfa" -> putStrLn "Executando DFA..."
        _     -> putStrLn "Tipo de autômato desconhecido!"

runNfae :: AutomatoDef -> IO ()
runNfae automatoDef = do
    let nfae = toNFAE automatoDef
    mostrarNFA nfae
    let nfa = nfaeParaNfa nfae
    print "----- Conversão de NFAE para NFA concluida! -----"
    mostrarNFA nfa
    automatoDefToFile (nfaToAutomatoDef nfa) "nfa_converted.yaml"

runNfa :: AutomatoDef -> IO ()
runNfa automatoDef = do
    let nfa = toNFA automatoDef
    mostrarNFA nfa
    let dfa = nfaParaDfa nfa
    print "----- Conversão de NFA para DFA concluida! -----"
    mostrarDFA dfa
    automatoDefToFile (dfaToAutomatoDef dfa) "dfa_converted.yaml"