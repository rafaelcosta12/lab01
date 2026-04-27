module Operations where
import qualified Data.Set as Set
import AutomatonTypes

epsilonDestinos :: [TransicaoNFA] -> Estado -> Set.Set Estado
epsilonDestinos transicoes estado = 
    Set.unions [tNFADestinos t | t <- transicoes,
                                 tNFAOrigem t == estado,
                                 tNFASimbolo t == "epsilon"]

fechoEpsilon :: [TransicaoNFA] -> Set.Set Estado -> Set.Set Estado
fechoEpsilon transicoes estados =
    fechoEpsilonAux transicoes (Set.toList estados) Set.empty

fechoEpsilonAux :: [TransicaoNFA] -> [Estado] -> Set.Set Estado -> Set.Set Estado

fechoEpsilonAux _ [] visitados =
    visitados

fechoEpsilonAux transicoes (atual:resto) visitados
    | atual `Set.member` visitados =
        fechoEpsilonAux transicoes resto visitados
    | otherwise =
        let novosEstados = Set.toList (epsilonDestinos transicoes atual)
        in fechoEpsilonAux transicoes
               (resto ++ novosEstados)
               (Set.insert atual visitados)

nfaeParaNfa :: NFA -> NFA
nfaeParaNfa NFA { nfaAlfabeto = alfabeto
                   , nfaEstados = estados
                   , nfaTransicoes = transicoes
                   , nfaInicial = inicial
                   , nfaFinais = finais
                   } =
    NFA { nfaAlfabeto = alfabetoNaoEpsilon
        , nfaEstados = estados
        , nfaTransicoes = novasTransicoes
        , nfaInicial = inicial
        , nfaFinais = novosFinais
        }
    where
        alfabetoNaoEpsilon = [s | s <- alfabeto, s /= "epsilon"]

        novasTransicoes =
            [TransicaoNFA
               {tNFAOrigem = q, tNFASimbolo = a, tNFADestinos = destino} |
               q <- estados,
               let fechoQ = fechoEpsilon transicoes (Set.singleton q),
               a <- alfabetoNaoEpsilon,
               let destinosDiretos
                     = Set.unions
                         [tNFADestinos t |
                            t <- transicoes,
                            tNFAOrigem t `Set.member` fechoQ,
                            tNFASimbolo t == a],
               not (Set.null destinosDiretos),
               let destino = fechoEpsilon transicoes destinosDiretos]

        novosFinais = Set.fromList
            [q | q <- estados,
                 not (Set.null (Set.intersection
                     (fechoEpsilon transicoes (Set.singleton q)) finais))]

calcularTransicao :: [TransicaoNFA] -> Set.Set Estado -> Simbolo -> TransicaoDFA
calcularTransicao transicoesNFA estadosOrigem simbolo =
    TransicaoDFA { tDFAOrigem = estadosOrigem
                 , tDFASimbolo = simbolo
                 , tDFADestino = destino
                 }
    where
        destinosDiretos =
            Set.unions [tNFADestinos t | estado <- Set.toList estadosOrigem,
                                         t <- transicoesNFA,
                                         tNFAOrigem t == estado,
                                         tNFASimbolo t == simbolo]

        destino = fechoEpsilon transicoesNFA destinosDiretos

gerarEstadosDFA :: [Simbolo] -> [TransicaoNFA] -> [Set.Set Estado]
               -> Set.Set (Set.Set Estado) -> [TransicaoDFA]
               -> ([Set.Set Estado], [TransicaoDFA])

gerarEstadosDFA _ _ [] processados transicoesDFA =
    (Set.toList processados, transicoesDFA)

gerarEstadosDFA alfabeto transicoesNFA (atual:fila) processados transicoesDFA
    | atual `Set.member` processados =
        gerarEstadosDFA alfabeto transicoesNFA fila processados transicoesDFA

    | otherwise =
        let novasTransicoes =
                [calcularTransicao transicoesNFA atual s | s <- alfabeto]

            novosEstados = [tDFADestino t | t <- novasTransicoes]

            estadosParaFila =
                [e | e <- novosEstados,
                     e `Set.notMember` processados,
                     e `notElem` fila,
                     e /= atual]

        in gerarEstadosDFA alfabeto transicoesNFA
               (fila ++ estadosParaFila)
               (Set.insert atual processados)
               (transicoesDFA ++ novasTransicoes)

nfaParaDfa :: NFA -> DFA
nfaParaDfa NFA { nfaAlfabeto = alfabeto
               , nfaTransicoes = transicoesNFA
               , nfaInicial = estadoInicial
               , nfaFinais = estadosFinais
               } =
    DFA { dfaAlfabeto = alfabetoDFA
        , dfaEstados = estadosDFA
        , dfaTransicoes = transicoesDFA
        , dfaInicial = estadoInicialDFA
        , dfaFinais = estadosFinaisDFA
        }

    where
        alfabetoDFA = [s | s <- alfabeto, s /= "epsilon"]

        estadoInicialDFA = fechoEpsilon transicoesNFA
                               (Set.singleton estadoInicial)

        (estadosDFA, transicoesDFA) =
            gerarEstadosDFA alfabetoDFA transicoesNFA
                            [estadoInicialDFA] Set.empty []

        estadosFinaisDFA = [estado | estado <- estadosDFA,
                                     not (Set.null (Set.intersection estado estadosFinais))]

-- ============================================================
-- FUNÇÕES PARA MOSTRAR OS RESULTADOS NA TELA
-- ============================================================

-- ---
-- FUNÇÃO: mostrarConjunto
-- ---
-- Converte uma lista de strings em uma representação legível
-- de conjunto: {a, b, c}
--
-- Exemplos:
--   mostrarConjunto Set.empty                 = "{}"
--   mostrarConjunto (Set.fromList ["q0"])     = "{q0}"
--   mostrarConjunto (Set.fromList ["q0","q1"]) = "{q0, q1}"

mostrarConjunto :: Set.Set String -> String
mostrarConjunto s = case Set.toList s of
    [] -> "{}"
    [x] -> "{" ++ x ++ "}"
    (x:xs) -> "{" ++ x ++ juntar xs
    where
        juntar [] = "}"
        juntar [y] = ", " ++ y ++ "}"
        juntar (y:ys) = ", " ++ y ++ juntar ys

-- ---
-- FUNÇÃO: mostrarTransicaoDFA
-- ---
-- Mostra uma transição do DFA de forma legível.
-- Exemplo:
--   mostrarTransicaoDFA TransicaoDFA { tDFAOrigem = Set.fromList ["q0","q1"]
--                                    , tDFASimbolo = "a"
--                                    , tDFADestino = Set.fromList ["q0","q2"]
--                                    }
--   resultado: "{q0, q1} --a--> {q0, q2}"

mostrarTransicaoDFA :: TransicaoDFA -> String
mostrarTransicaoDFA TransicaoDFA { tDFAOrigem = origem
                                , tDFASimbolo = simbolo
                                , tDFADestino = destino
                                } =
    mostrarConjunto origem ++ " --" ++ simbolo ++ "--> " ++ mostrarConjunto destino

-- ---
-- FUNÇÃO: mostrarDFA
-- ---
-- Mostra o DFA completo na tela de forma organizada.

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

-- ---
-- FUNÇÃO: mostrarNFA
-- ---
-- Mostra o NFA original na tela.

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

-- ============================================================
-- FUNÇÃO PARA TESTAR SE UM DFA ACEITA UMA STRING
-- ============================================================
--
-- Simula a execução do DFA em uma lista de símbolos.
-- O DFA começa no estado inicial e, para CADA símbolo
-- da entrada, segue a transição correspondente.
-- No final, verifica se o estado atual é final.
--
-- Esta função SÓ funciona porque o DFA é determinístico:
-- para cada estado e símbolo existe NO MÁXIMO UMA transição.

-- ---
-- FUNÇÃO: lookupTransicao
-- ---
-- Procura uma transição em uma lista de transições do DFA.
--
-- Parâmetros:
--   transicoes: lista de transições para procurar
--   estado:     o estado de origem (conjunto de estados do NFA)
--   simbolo:    o símbolo da transição
--
-- Retorno:
--   Nothing          -> não encontrou transição
--   Just destino     -> encontrou, retorna o destino
--
-- Esta função percorre a lista linearmente.
-- Como o DFA é determinístico, no máximo UMA transição
-- vai corresponder.

lookupTransicao :: [TransicaoDFA] -> Set.Set Estado -> Simbolo -> Maybe (Set.Set Estado)
lookupTransicao [] _ _ =
    Nothing

lookupTransicao (TransicaoDFA { tDFAOrigem = origem
                              , tDFASimbolo = s
                              , tDFADestino = destino
                              } : resto) estadoAtual simbolo
    | origem == estadoAtual && s == simbolo = Just destino
    | otherwise = lookupTransicao resto estadoAtual simbolo

-- ---
-- FUNÇÃO: aceitarDFAAux
-- ---
-- Função auxiliar que processa a entrada símbolo por símbolo.

aceitarDFAAux :: [TransicaoDFA] -> [Set.Set Estado] -> Set.Set Estado -> [Simbolo] -> Bool

-- Caso 1: a entrada acabou.
-- Verifica se o estado atual é um estado final do DFA.
aceitarDFAAux _ finais estadoAtual [] =
    estadoAtual `elem` finais

-- Caso 2: ainda há símbolos para ler.
aceitarDFAAux transicoes finais estadoAtual (simbolo:resto) =
    case lookupTransicao transicoes estadoAtual simbolo of
        -- Se não encontrou transição, a string é rejeitada
        Nothing -> False
        -- Se encontrou, continua a partir do próximo estado
        Just proxEstado -> aceitarDFAAux transicoes finais proxEstado resto

-- ---
-- FUNÇÃO: aceitarDFA
-- ---
-- Verifica se um DFA aceita uma determinada entrada.
--
-- Parâmetros:
--   dfa: o DFA a ser testado
--   entrada: lista de símbolos (cada símbolo é uma String)
--
-- Retorno: True se o DFA aceita, False caso contrário.

aceitarDFA :: DFA -> [Simbolo] -> Bool
aceitarDFA DFA { dfaTransicoes = transicoes, dfaInicial = inicial, dfaFinais = finais } = aceitarDFAAux transicoes finais inicial

-- ============================================================
-- FUNÇÃO PRINCIPAL (main)
-- ============================================================
-- Contém exemplos para testar o conversor.

main :: IO ()
main = do
    -- ============================================================
    -- EXEMPLO 1:
    -- NFA que reconhece strings terminadas em "ab"
    -- ============================================================
    --
    -- Estados: q0, q1, q2
    -- Alfabeto: a, b
    -- Transições:
    --   q0 --a--> q0   (continua lendo 'a')
    --   q0 --b--> q0   (continua lendo 'b')
    --   q0 --a--> q1   (começou padrão "ab")
    --   q1 --b--> q2   (completou "ab"!)
    -- Inicial: q0
    -- Final:   q2
    --
    -- Strings aceitas: "ab", "aab", "bab", "aaab", "bbab", ...
    -- Strings rejeitadas: "", "a", "b", "ba", "aba", ...

    putStrLn "========================================"
    putStrLn "EXEMPLO 1: Strings que terminam com 'ab'"
    putStrLn "========================================"
    putStrLn ""

    let alfabeto1 = ["a", "b"]
    let estados1  = ["q0", "q1", "q2"]
    let transicoes1 =
            [ TransicaoNFA { tNFAOrigem = "q0", tNFASimbolo = "a", tNFADestinos = Set.fromList ["q0", "q1"] }
            , TransicaoNFA { tNFAOrigem = "q0", tNFASimbolo = "b", tNFADestinos = Set.fromList ["q0"] }
            , TransicaoNFA { tNFAOrigem = "q1", tNFASimbolo = "b", tNFADestinos = Set.fromList ["q2"] }
            ]
    let nfa1 = NFA { nfaAlfabeto = alfabeto1
                   , nfaEstados = estados1
                   , nfaTransicoes = transicoes1
                   , nfaInicial = "q0"
                   , nfaFinais = Set.fromList ["q2"]
                   }

    mostrarNFA nfa1

    let dfa1 = nfaParaDfa nfa1
    mostrarDFA dfa1

    putStrLn ""
    putStrLn "----------------------------------------"
    putStrLn "Testando o DFA com algumas strings:"
    putStrLn "----------------------------------------"
    putStrLn $ "  \"ab\"   -> " ++ show (aceitarDFA dfa1 ["a","b"])
    putStrLn $ "  \"aab\"  -> " ++ show (aceitarDFA dfa1 ["a","a","b"])
    putStrLn $ "  \"bab\"  -> " ++ show (aceitarDFA dfa1 ["b","a","b"])
    putStrLn $ "  \"\"     -> " ++ show (aceitarDFA dfa1 [])
    putStrLn $ "  \"ba\"   -> " ++ show (aceitarDFA dfa1 ["b","a"])
    putStrLn $ "  \"aba\"  -> " ++ show (aceitarDFA dfa1 ["a","b","a"])
    putStrLn $ "  \"a\"    -> " ++ show (aceitarDFA dfa1 ["a"])
    putStrLn ""

    -- ============================================================
    -- EXEMPLO 2:
    -- NFA COM TRANSIÇÕES ε (epsilon)
    -- Reconhece: zero ou mais 'a' seguidos de UM 'b'
    -- ============================================================
    --
    -- Este NFA mostra a importância do fecho-ε.
    -- A transição q0 --ε--> q1 permite que o NFA comece
    -- em q1 sem ler nada.
    --
    -- Estados: q0, q1, q2
    -- Alfabeto: a, b, ε
    -- Transições:
    --   q0 --ε--> q1   (vai para q1 sem ler nada)
    --   q1 --a--> q1   (lê 'a' e fica em q1)
    --   q1 --b--> q2   (lê 'b' e vai para q2)
    -- Inicial: q0
    -- Final:   q2
    --
    -- Aceitas: "b", "ab", "aab", "aaab", ...
    -- Rejeitadas: "", "a", "ba", "aba", ...

    putStrLn "========================================"
    putStrLn "EXEMPLO 2: NFA com transicoes epsilon"
    putStrLn "          (zero ou mais 'a' + um 'b')"
    putStrLn "========================================"
    putStrLn ""

    let alfabeto2 = ["a", "b", "epsilon"]
    let estados2  = ["q0", "q1", "q2"]
    let transicoes2 =
            [ TransicaoNFA { tNFAOrigem = "q0", tNFASimbolo = "epsilon", tNFADestinos = Set.fromList ["q1"] }
            , TransicaoNFA { tNFAOrigem = "q1", tNFASimbolo = "a", tNFADestinos = Set.fromList ["q1"] }
            , TransicaoNFA { tNFAOrigem = "q1", tNFASimbolo = "b", tNFADestinos = Set.fromList ["q2"] }
            ]
    let nfa2 = NFA { nfaAlfabeto = alfabeto2
                   , nfaEstados = estados2
                   , nfaTransicoes = transicoes2
                   , nfaInicial = "q0"
                   , nfaFinais = Set.fromList ["q2"]
                   }

    mostrarNFA nfa2

    let dfa2 = nfaParaDfa nfa2
    mostrarDFA dfa2

    putStrLn ""
    putStrLn "----------------------------------------"
    putStrLn "Testando o DFA com algumas strings:"
    putStrLn "----------------------------------------"
    putStrLn $ "  \"b\"    -> " ++ show (aceitarDFA dfa2 ["b"])
    putStrLn $ "  \"ab\"   -> " ++ show (aceitarDFA dfa2 ["a","b"])
    putStrLn $ "  \"aab\"  -> " ++ show (aceitarDFA dfa2 ["a","a","b"])
    putStrLn $ "  \"\"     -> " ++ show (aceitarDFA dfa2 [])
    putStrLn $ "  \"a\"    -> " ++ show (aceitarDFA dfa2 ["a"])
    putStrLn $ "  \"ba\"   -> " ++ show (aceitarDFA dfa2 ["b","a"])

    -- ============================================================
    -- EXEMPLO 3:
    -- Removendo ε-transições com removerEpsilon
    -- ============================================================
    --
    -- Mostra o NFA-ε do exemplo 2 após remover os movimentos
    -- vazios, e confirma que o DFA resultante é idêntico.

    putStrLn ""
    putStrLn "========================================"
    putStrLn "EXEMPLO 3: Removendo transicoes epsilon"
    putStrLn "========================================"
    putStrLn ""

    let nfaSemEpsilon = nfaeParaNfa nfa2
    mostrarNFA nfaSemEpsilon

    let dfa3 = nfaParaDfa nfaSemEpsilon
    mostrarDFA dfa3

    putStrLn ""
    putStrLn "----------------------------------------"
    putStrLn "Testando (idêntico ao exemplo 2):"
    putStrLn "----------------------------------------"
    putStrLn $ "  \"b\"    -> " ++ show (aceitarDFA dfa3 ["b"])
    putStrLn $ "  \"ab\"   -> " ++ show (aceitarDFA dfa3 ["a","b"])
    putStrLn $ "  \"aab\"  -> " ++ show (aceitarDFA dfa3 ["a","a","b"])
    putStrLn $ "  \"\"     -> " ++ show (aceitarDFA dfa3 [])
    putStrLn $ "  \"a\"    -> " ++ show (aceitarDFA dfa3 ["a"])
    putStrLn $ "  \"ba\"   -> " ++ show (aceitarDFA dfa3 ["b","a"])
