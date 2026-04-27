module RegexParser where

import RegexTypes (Reg(..))

parse :: String -> Reg
parse "" = Epsilon
parse s = reg
    where (reg, _) = parseRegex s

parseRegex :: String -> (Reg, String)
parseRegex s = (regAposPipe, restoFinal)
    where
        (reg, resto) = parseTerm s
        (regAposPipe, restoFinal) = aposPipe reg resto

aposPipe :: Reg -> String -> (Reg, String)
aposPipe r ('|' : s) = (Or r r2, rest2)
    where (r2, rest2) = parseRegex s
aposPipe r s = (r, s)

parseTerm :: String -> (Reg, String)
parseTerm s = combinaFatores (parseFactors s)
    where
        combinaFatores ([], resto) = (Epsilon, resto)
        combinaFatores (rs, resto) = (foldl1 Then rs, resto)

parseFactors :: String -> ([Reg], String)
parseFactors ""        = ([], "")
parseFactors ('|' : s) = ([], '|' : s)
parseFactors (')' : s) = ([], ')' : s)
parseFactors s         = (r : rs, resto2)
    where
        (r, resto1) = parseFactor s
        (rs, resto2) = parseFactors resto1

parseFactor :: String -> (Reg, String)
parseFactor ('(' : s) = fechaParen (parseRegex s)
    where
        fechaParen (r, ')' : resto') = aplicaPosfixo r resto'
        fechaParen _                 = error "Parse error: parentese nao fechado"
        aplicaPosfixo r ('?' : resto) = (Or r Epsilon, resto)
        aplicaPosfixo r ('*' : resto) = (Star r, resto)
        aplicaPosfixo r ('+' : resto) = (Then r (Star r), resto)
        aplicaPosfixo r resto         = (r, resto)
parseFactor (c : '?' : s) = (Or (Literal c) Epsilon, s)
parseFactor (c : '*' : s) = (Star (Literal c), s)
parseFactor (c : '+' : s) = (Then (Literal c) (Star (Literal c)), s)
parseFactor [c]           = (Literal c, "")
parseFactor (c : s)       = (Literal c, s)
parseFactor []            = error "Parse error: fim de entrada inesperado"
