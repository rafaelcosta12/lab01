module AutomatonTypes where

import qualified Data.Set as Set

type Estado = String

type Simbolo = String

data TransicaoNFA = TransicaoNFA
    { tNFAOrigem   :: Estado
    , tNFASimbolo  :: Simbolo
    , tNFADestinos :: Set.Set Estado
    } deriving (Show)

data NFA = NFA
    { nfaAlfabeto   :: [Simbolo]
    , nfaEstados    :: [Estado]
    , nfaTransicoes :: [TransicaoNFA]
    , nfaInicial    :: Estado
    , nfaFinais     :: Set.Set Estado
    } deriving (Show)

data TransicaoDFA = TransicaoDFA
    { tDFAOrigem  :: Set.Set Estado
    , tDFASimbolo :: Simbolo
    , tDFADestino :: Set.Set Estado
    } deriving (Show)

data DFA = DFA
    { dfaAlfabeto   :: [Simbolo]
    , dfaEstados    :: [Set.Set Estado]
    , dfaTransicoes :: [TransicaoDFA]
    , dfaInicial    :: Set.Set Estado
    , dfaFinais     :: [Set.Set Estado]
    } deriving (Show)
