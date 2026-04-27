module Display where
import qualified Data.Set as Set
import AutomatonTypes

mostrarConjunto :: Set.Set String -> String
mostrarConjunto s = case Set.toList s of
    [] -> "{}"
    [x] -> "{" ++ x ++ "}"
    (x:xs) -> "{" ++ x ++ juntar xs
    where
        juntar [] = "}"
        juntar [y] = ", " ++ y ++ "}"
        juntar (y:ys) = ", " ++ y ++ juntar ys

mostrarTransicaoDFA :: TransicaoDFA -> String
mostrarTransicaoDFA TransicaoDFA { tDFAOrigem = origem
                                , tDFASimbolo = simbolo
                                , tDFADestino = destino
                                } =
    mostrarConjunto origem ++ " --" ++ simbolo ++ "--> " ++ mostrarConjunto destino

mostrarDFA :: DFA -> IO ()
mostrarDFA DFA { dfaAlfabeto = alfabeto
              , dfaEstados = estados
              , dfaTransicoes = transicoes
              , dfaInicial = inicial
              , dfaFinais = finais
              } = do
    putStrLn $ "Alfabeto: " ++ show alfabeto
    putStrLn "Estados do DFA (cada um é um conjunto de estados do NFA):"
    mapM_ (\e -> putStrLn $ "  " ++ mostrarConjunto e) estados
    putStrLn ""
    putStrLn $ "Estado inicial: " ++ mostrarConjunto inicial
    putStrLn ""
    putStrLn "Estados finais:"
    mapM_ (\f -> putStrLn $ "  " ++ mostrarConjunto f) finais
    putStrLn ""
    putStrLn "Transicoes:"
    mapM_ (\t -> putStrLn $ "  " ++ mostrarTransicaoDFA t) transicoes

mostrarNFA :: NFA -> IO ()
mostrarNFA NFA { nfaAlfabeto = alfabeto
              , nfaEstados = estados
              , nfaTransicoes = transicoes
              , nfaInicial = inicial
              , nfaFinais = finais
              } = do
    putStrLn ""
    putStrLn $ "Alfabeto: " ++ show alfabeto
    putStrLn $ "Estados: " ++ show estados
    putStrLn "Transicoes:"
    mapM_ print transicoes
    putStrLn $ "Estado inicial: " ++ inicial
    putStrLn $ "Estados finais: " ++ show finais
    putStrLn ""
