module RegularExpressions where
import AutomatonTypes (NFA(..), TransicaoNFA (..), Simbolo)
import qualified Data.Set as Set

regexConcat :: NFA -> NFA -> NFA
regexConcat nfa1 nfa2 = NFA {
    nfaAlfabeto   = Set.toList (Set.fromList (nfaAlfabeto nfa1 ++ nfaAlfabeto nfa2)),
    nfaEstados    = Set.toList (Set.union (Set.fromList (nfaEstados nfa1)) (Set.fromList (nfaEstados nfa2))),
    nfaTransicoes = nfaTransicoes nfa1 ++ nfaTransicoes nfa2 ++ novasTransicoes,
    nfaInicial    = nfaInicial nfa1,
    nfaFinais     = nfaFinais nfa2 }
    where novasTransicoes = [ TransicaoNFA {
        tNFAOrigem = estadoFinal,
        tNFASimbolo = "epsilon",
        tNFADestinos = Set.singleton (nfaInicial nfa2)
    } | estadoFinal <- Set.toList (nfaFinais nfa1)]

regexUnion :: NFA -> NFA -> NFA
regexUnion nfa1 nfa2 = NFA {
    nfaAlfabeto   = Set.toList (Set.fromList (nfaAlfabeto nfa1 ++ nfaAlfabeto nfa2)),
    nfaEstados    = Set.toList (Set.union (Set.fromList (nfaEstados nfa1)) (Set.fromList (nfaEstados nfa2))) ++ novosEstados,
    nfaTransicoes = nfaTransicoes nfa1 ++ nfaTransicoes nfa2 ++ novasTransicoes,
    nfaInicial = estado1,
    nfaFinais = Set.singleton estado2 }
    where
        estado1 = "+" ++ nfaInicial nfa1 ++ nfaInicial nfa2
        estado2 = nfaInicial nfa1 ++ nfaInicial nfa2 ++ "+"
        novosEstados = [estado1, estado2]
        transicoesDoInicio = [TransicaoNFA {
                                tNFAOrigem = estado1,
                                tNFASimbolo = "epsilon",
                                tNFADestinos = Set.singleton i
                            } | i <- [nfaInicial nfa1, nfaInicial nfa2]]
        transicoesDoFinal = [TransicaoNFA {
                                tNFAOrigem = i,
                                tNFASimbolo = "epsilon",
                                tNFADestinos = Set.singleton estado2
                            } | i <- Set.toList (Set.union (nfaFinais nfa1) (nfaFinais nfa2))]
        novasTransicoes = transicoesDoInicio ++ transicoesDoFinal

regexStar :: NFA -> NFA
regexStar nfa = NFA {
    nfaAlfabeto   = nfaAlfabeto nfa,
    nfaEstados    = nfaEstados nfa ++ novosEstados,
    nfaTransicoes = nfaTransicoes nfa ++ novasTransicoes,
    nfaInicial = estadoInicial,
    nfaFinais = Set.singleton estadoFinal }
    where
        estadoInicial = "*" ++ nfaInicial nfa
        estadoFinal = concat (Set.toList (nfaFinais nfa)) ++ "*"
        novosEstados = [estadoInicial, estadoFinal]
        transicao1 = TransicaoNFA {
            tNFAOrigem = estadoInicial,
            tNFASimbolo = "epsilon",
            tNFADestinos = Set.singleton estadoFinal }
        transicao2 = TransicaoNFA {
            tNFAOrigem = estadoInicial,
            tNFASimbolo = "epsilon",
            tNFADestinos = Set.singleton (nfaInicial nfa) }
        transicao3 = [TransicaoNFA {
            tNFAOrigem = f,
            tNFASimbolo = "epsilon",
            tNFADestinos = Set.singleton estadoFinal } | f <- Set.toList (nfaFinais nfa)]
        transicao4 = [TransicaoNFA {
            tNFAOrigem = f,
            tNFASimbolo = "epsilon",
            tNFADestinos = Set.singleton (nfaInicial nfa) } | f <- Set.toList (nfaFinais nfa)]
        novasTransicoes = [transicao1, transicao2] ++ transicao3 ++ transicao4

regexSingle :: Simbolo -> NFA
regexSingle simbolo = NFA {
    nfaAlfabeto = [simbolo],
    nfaEstados = estados,
    nfaTransicoes = transicoes,
    nfaInicial = estado1,
    nfaFinais = Set.singleton estado2 }
    where
        estado1 = simbolo ++ "0"
        estado2 = simbolo ++ "1"
        estados = [estado1, estado2]
        transicao1 = TransicaoNFA {
            tNFAOrigem = estado1,
            tNFASimbolo = simbolo,
            tNFADestinos = Set.singleton estado2 }
        transicoes = [transicao1]

data Reg = Epsilon |
           Literal Char |
           Or Reg Reg |
           Then Reg Reg |
           Star Reg
           deriving (Eq, Show)

build :: Reg -> NFA
build (Literal c) = regexSingle (show c)
build (Or r1 r2) = regexUnion (build r1) (build r2)
build (Then r1 r2) = regexConcat (build r1) (build r2)
build (Star r) = regexStar (build r)
build Epsilon = NFA { nfaAlfabeto = [], nfaEstados = [], nfaTransicoes = [], nfaInicial = "0", nfaFinais = Set.singleton "0" }

-- | PARSER: converte String -> Reg (árvore sintática).
--
-- Cada função retorna (Reg, sobra) onde "sobra" é o resto não processado.
-- Isso permite que cada nível "coma" só o que lhe pertence e devolva
-- o resto para o nível acima.
--
-- Gramática (precedência crescente):
--   regex   → term ('|' term)*      (Or  — menor precedência)
--   term    → factor+                (Then — concatenação implícita)
--   factor  → primary '*'?           (Star — maior precedência)
--   primary → char | '(' regex ')'   (Literal ou grupo)

-- | Ponto de entrada
parse :: String -> Reg
parse "" = Epsilon
parse s = reg
    where (reg, _) = parseRegex s

-- | regex   → term ('|' term)*
-- Parseia um term, depois vê se vem '|' e mais um regex
parseRegex :: String -> (Reg, String)
parseRegex s = (regAposPipe, restoFinal)
    where
        (reg, resto) = parseTerm s
        (regAposPipe, restoFinal) = aposPipe reg resto

-- | aposPipe: depois de um term, se vier '|', parseia o próximo regex
aposPipe :: Reg -> String -> (Reg, String)
aposPipe r ('|' : s) = (Or r r2, rest2)
    where (r2, rest2) = parseRegex s
aposPipe r s = (r, s)

-- | term   → factor+
-- Parseia o máximo de fatores em sequência e junta com Then
parseTerm :: String -> (Reg, String)
parseTerm s = combinaFatores (parseFactors s)
    where
        combinaFatores ([], resto)   = (Epsilon, resto)
        combinaFatores (rs, resto)   = (foldl1 Then rs, resto)

-- | parseFactors: lê o máximo de fatores em sequência
parseFactors :: String -> ([Reg], String)
parseFactors ""           = ([], "")
parseFactors ('|' : s)    = ([], '|' : s)
parseFactors (')' : s)    = ([], ')' : s)
parseFactors s            = (r : rs, resto2)
    where
        (r, resto1) = parseFactor s
        (rs, resto2) = parseFactors resto1

-- | factor  → primary '*'?
-- Parseia um átomo (literal ou grupo) e, se vier '*', aplica Star
parseFactor :: String -> (Reg, String)
parseFactor ('(' : s) = fechaParen (parseRegex s)
    where
        fechaParen (r, ')':resto')  = aplicaEstrela r resto'
        fechaParen _                = error "Parse error: parêntese não fechado"
        aplicaEstrela r ('*':resto) = (Star r, resto)
        aplicaEstrela r resto       = (r, resto)
parseFactor (c : '*' : s) = (Star (Literal c), s)
parseFactor [c]           = (Literal c, "")
parseFactor (c : s)       = (Literal c, s)
parseFactor []            = error "Parse error: fim de entrada inesperado"
