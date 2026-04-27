module Acceptance where
import qualified Data.Set as Set
import AutomatonTypes

lookupTransicao :: [TransicaoDFA] -> Set.Set Estado -> Simbolo -> Maybe (Set.Set Estado)
lookupTransicao [] _ _ =
    Nothing

lookupTransicao (TransicaoDFA { tDFAOrigem = origem
                              , tDFASimbolo = s
                              , tDFADestino = destino
                              } : resto) estadoAtual simbolo
    | origem == estadoAtual && s == simbolo = Just destino
    | otherwise = lookupTransicao resto estadoAtual simbolo

aceitarDFAAux :: [TransicaoDFA] -> [Set.Set Estado] -> Set.Set Estado -> [Simbolo] -> Bool

aceitarDFAAux _ finais estadoAtual [] =
    estadoAtual `elem` finais

aceitarDFAAux transicoes finais estadoAtual (simbolo:resto) =
    case lookupTransicao transicoes estadoAtual simbolo of
        Nothing -> False
        Just proxEstado -> aceitarDFAAux transicoes finais proxEstado resto

aceitarDFA :: DFA -> [Simbolo] -> Bool
aceitarDFA DFA { dfaTransicoes = transicoes, dfaInicial = inicial, dfaFinais = finais } = aceitarDFAAux transicoes finais inicial
