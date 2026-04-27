{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module ParseYAML where

import Data.Yaml
import qualified Data.Text as T
import qualified Data.Vector as V
import AutomatonTypes
import qualified Data.Set as Set
import Data.Text.Encoding

data TransitionDef = TransitionDef
  { origem :: T.Text
  , simbolo :: T.Text
  , destinos :: [T.Text]
  } deriving (Show)

data AutomatoDef = AutomatoDef
  { autType        :: T.Text
  , autAlphabet    :: [T.Text]
  , autStates      :: [T.Text]
  , autInitial     :: T.Text
  , autFinals      :: [T.Text]
  , autTransitionDefs :: [TransitionDef]
  } deriving (Show)

instance FromJSON TransitionDef where
    parseJSON :: Value -> Parser TransitionDef
    parseJSON (Object v) =
        TransitionDef
            <$> v .: "from"
            <*> (v .: "symbol" >>= valueToText)
            <*> (v .: "to" >>= parseTextFieldArray)
    parseJSON _ = fail "Expected an object for TransitionDef"

instance FromJSON AutomatoDef where
    parseJSON :: Value -> Parser AutomatoDef
    parseJSON (Object v) =
        AutomatoDef
            <$> v .: "type"
            <*> (v .: "alphabet" >>= parseTextFieldArray)
            <*> (v .: "states" >>= parseTextFieldArray)
            <*> v .: "initial_state"
            <*> (v .: "final_states" >>= parseTextFieldArray)
            <*> v .: "transitions"
    parseJSON _ = fail "Expected an object for AutomatoDef"

instance ToJSON TransitionDef where
    toJSON :: TransitionDef -> Value
    toJSON (TransitionDef orig simb dests) =
        object [ "from" .= orig
               , "symbol" .= simb
               , "to" .= dests
               ]
               
instance ToJSON AutomatoDef where
    toJSON :: AutomatoDef -> Value
    toJSON (AutomatoDef t alphabet states initial finals transitions) =
        object [ "type" .= t
               , "alphabet" .= alphabet
               , "states" .= states
               , "initial_state" .= initial
               , "final_states" .= finals
               , "transitions" .= transitions
               ]


parseTextFieldArray :: Value -> Parser [T.Text]
parseTextFieldArray (Array arr) = mapM valueToText (V.toList arr)
parseTextFieldArray _           = fail "expected array"

valueToText :: Value -> Parser T.Text
valueToText (String t) = pure t
valueToText (Number n) = pure $ T.pack (show n)
valueToText _          = fail "expected string or number"

readYAMLFile :: FilePath -> IO AutomatoDef
readYAMLFile filePath = do
    result <- decodeFileEither filePath :: IO (Either ParseException AutomatoDef)
    case result of
        Left err -> error $ "Error parsing YAML file: " ++ show err
        Right automatoDef -> return automatoDef

toDFA :: AutomatoDef -> DFA
toDFA automatoDef = DFA {
    dfaAlfabeto = [T.unpack s | s <- autAlphabet automatoDef, s /= "epsilon"],
    dfaEstados = [Set.fromList [ T.unpack k | k <- autStates automatoDef]],
    dfaTransicoes = [
        TransicaoDFA { 
            tDFAOrigem = Set.singleton (T.unpack (origem t)), 
            tDFASimbolo = T.unpack (simbolo t), 
            tDFADestino = Set.fromList [ T.unpack d | d <- destinos t ] 
        } | t <- autTransitionDefs automatoDef],
    dfaInicial = Set.empty,
    dfaFinais = [Set.fromList [ T.unpack k | k <- autFinals automatoDef]]
}

toNFA :: AutomatoDef -> NFA
toNFA automatoDef = NFA {
    nfaAlfabeto = [T.unpack s | s <- autAlphabet automatoDef, s /= "epsilon"],
    nfaEstados = [T.unpack s | s <- autStates automatoDef],
    nfaTransicoes = [
        TransicaoNFA { 
            tNFAOrigem = T.unpack (origem t), 
            tNFASimbolo = T.unpack (simbolo t), 
            tNFADestinos = Set.fromList [ T.unpack d | d <- destinos t ] 
        } | t <- autTransitionDefs automatoDef],
    nfaInicial = T.unpack (autInitial automatoDef),
    nfaFinais = Set.fromList [ T.unpack k | k <- autFinals automatoDef]
}

toNFAE :: AutomatoDef -> NFA
toNFAE automatoDef = NFA {
    nfaAlfabeto = [T.unpack s | s <- autAlphabet automatoDef] ++ ["epsilon"],
    nfaEstados = [T.unpack s | s <- autStates automatoDef],
    nfaTransicoes = [
        TransicaoNFA { 
            tNFAOrigem = T.unpack (origem t), 
            tNFASimbolo = T.unpack (simbolo t), 
            tNFADestinos = Set.fromList [ T.unpack d | d <- destinos t ] 
        } | t <- autTransitionDefs automatoDef],
    nfaInicial = T.unpack (autInitial automatoDef),
    nfaFinais = Set.fromList [ T.unpack k | k <- autFinals automatoDef]
}

nfaToAutomatoDef :: NFA -> AutomatoDef
nfaToAutomatoDef nfa = AutomatoDef {
    autType = "nfae",
    autAlphabet = map T.pack (nfaAlfabeto nfa) ++ ["epsilon"],
    autStates = map T.pack (nfaEstados nfa),
    autInitial = T.pack (nfaInicial nfa),
    autFinals = map T.pack (Set.toList (nfaFinais nfa)),
    autTransitionDefs = [ TransitionDef {
        origem = T.pack (tNFAOrigem t),
        simbolo = T.pack (tNFASimbolo t),
        destinos = map T.pack (Set.toList (tNFADestinos t))
    } | t <- nfaTransicoes nfa]
}

dfaToAutomatoDef :: DFA -> AutomatoDef
dfaToAutomatoDef dfa = AutomatoDef {
    autType = "dfa",
    autAlphabet = map T.pack (dfaAlfabeto dfa),
    autStates = [T.pack (concat (Set.toList s)) | s <- dfaEstados dfa],
    autInitial = T.pack (concat (Set.toList (dfaInicial dfa))),
    autFinals = [T.pack (concat (Set.toList s)) | s <- dfaFinais dfa],
    autTransitionDefs = [ TransitionDef {
        origem = T.pack (concat (Set.toList (tDFAOrigem t))),
        simbolo = T.pack (tDFASimbolo t),
        destinos = [T.pack (concat (Set.toList (tDFADestino t)))]
    } | t <- dfaTransicoes dfa]
}

automatoDefToFile :: AutomatoDef -> FilePath -> IO ()
automatoDefToFile automatoDef filePath = do
    let yamlData = encode automatoDef
    writeFile filePath (T.unpack (decodeUtf8 yamlData))