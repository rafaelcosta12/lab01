module Conversion where

import qualified Data.Set as Set
import AutomatonTypes

epsilonDestinos :: [TransicaoNFA] -> Estado -> Set.Set Estado
epsilonDestinos transicoes estado =
    Set.unions [tNFADestinos t | t <- transicoes
               , tNFAOrigem t == estado
               , tNFASimbolo t == "epsilon"]

fechoEpsilon :: [TransicaoNFA] -> Set.Set Estado -> Set.Set Estado
fechoEpsilon transicoes estados =
    fechoEpsilonAux transicoes (Set.toList estados) Set.empty

fechoEpsilonAux :: [TransicaoNFA] -> [Estado] -> Set.Set Estado -> Set.Set Estado

fechoEpsilonAux _ [] visitados =
    visitados

fechoEpsilonAux transicoes (atual : resto) visitados
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
                { tNFAOrigem = q, tNFASimbolo = a, tNFADestinos = destino }
            | q <- estados
            , let fechoQ = fechoEpsilon transicoes (Set.singleton q)
            , a <- alfabetoNaoEpsilon
            , let destinosDiretos =
                    Set.unions
                        [tNFADestinos t | t <- transicoes
                        , tNFAOrigem t `Set.member` fechoQ
                        , tNFASimbolo t == a]
            , not (Set.null destinosDiretos)
            , let destino = fechoEpsilon transicoes destinosDiretos]

        novosFinais = Set.fromList
            [q | q <- estados
            , not (Set.null (Set.intersection
                (fechoEpsilon transicoes (Set.singleton q)) finais))]

calcularTransicao :: [TransicaoNFA] -> Set.Set Estado -> Simbolo -> TransicaoDFA
calcularTransicao transicoesNFA estadosOrigem simbolo =
    TransicaoDFA { tDFAOrigem = estadosOrigem
                 , tDFASimbolo = simbolo
                 , tDFADestino = destino
                 }
    where
        destinosDiretos =
            Set.unions [tNFADestinos t | estado <- Set.toList estadosOrigem
                        , t <- transicoesNFA
                        , tNFAOrigem t == estado
                        , tNFASimbolo t == simbolo]

        destino = fechoEpsilon transicoesNFA destinosDiretos

gerarEstadosDFA :: [Simbolo] -> [TransicaoNFA] -> [Set.Set Estado]
                -> Set.Set (Set.Set Estado) -> [TransicaoDFA]
                -> ([Set.Set Estado], [TransicaoDFA])

gerarEstadosDFA _ _ [] processados transicoesDFA =
    (Set.toList processados, transicoesDFA)

gerarEstadosDFA alfabeto transicoesNFA (atual : fila) processados transicoesDFA
    | atual `Set.member` processados =
        gerarEstadosDFA alfabeto transicoesNFA fila processados transicoesDFA
    | otherwise =
        let novasTransicoes =
                [calcularTransicao transicoesNFA atual s | s <- alfabeto]

            novosEstados = [tDFADestino t | t <- novasTransicoes]

            estadosParaFila =
                [e | e <- novosEstados
                , e `Set.notMember` processados
                , e `notElem` fila
                , e /= atual]

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

        estadosFinaisDFA = [estado | estado <- estadosDFA
                           , not (Set.null
                               (Set.intersection estado estadosFinais))]
