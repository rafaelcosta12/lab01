# Lab01 — Autômatos e Expressões Regulares

Conversor de autômatos (NFAE → NFA → DFA) e construtor de NFA a partir de expressões regulares, escrito em Haskell.

## Estrutura do Projeto

```
lab01/
├── lab01.cabal                  # Definição do pacote (1 library + 2 executáveis)
├── src/                         # Biblioteca compartilhada
│   ├── AutomatonTypes.hs        # Tipos dos autômatos (NFA, DFA, etc.)
│   ├── Conversion.hs            # Conversão entre autômatos (fecho-ε, NFAE→NFA, NFA→DFA)
│   ├── Display.hs               # Exibição dos autômatos na tela
│   └── Acceptance.hs            # Teste de aceitação de strings no DFA
├── app/
│   ├── converter/
│   │   ├── Main.hs              # Executável: conversor de autômatos
│   │   └── ParseYAML.hs         # Leitura/escrita de arquivos YAML
│   └── regex/
│       ├── Main.hs              # Executável: expressões regulares
│       ├── RegexTypes.hs        # Tipo Reg e construção do NFA para cada operação
│       └── RegexParser.hs       # Parser de expressões regulares
├── input.yaml                   # Exemplo de autômato NFAE
└── input2.yaml                  # Exemplo de autômato NFA
```

## Como compilar e executar

```bash
cabal build                              # compila tudo
cabal run converter -- input.yaml        # converte autômato do YAML
cabal run converter -- input.yaml "01"   # converte e testa string
cabal run regex -- "a|b*"                # constrói NFA da regex
cabal run regex -- "a|b*" "abb"          # constrói NFA e testa string
```

## Funcionalidades

### Conversor de autômatos (`converter`)

Lê um arquivo YAML descrevendo um autômato e:
- Se for **NFAE**: remove transições epsilon (NFAE → NFA) e converte para DFA
- Se for **NFA**: converte para DFA via construção de subconjuntos
- Se for **DFA**: exibe o autômato
- Opcionalmente testa uma string no DFA resultante

### Expressões regulares (`regex`)

Converte uma expressão regular em um NFA equivalente. Operadores suportados:
- `a` — caractere literal
- `ab` — concatenação
- `a|b` — união (alternância)
- `a*` — zero ou mais repetições (estrela de Kleene)
- `a+` — uma ou mais repetições (desaçucarado para `aa*`)
- `a?` — opcional (desaçucarado para `a|ε`)
- `( )` — agrupamento

## Formato do arquivo YAML

```yaml
type: nfae             # dfa | nfa | nfae
alphabet: ['0', '1']
states: [q0, q1, q2]
initial_state: q0
final_states: [q2]
transitions:
  - from: q0
    symbol: '0'
    to: [q0, q1]
  - from: q0
    symbol: epsilon
    to: [q1]
  - from: q1
    symbol: '1'
    to: [q2]
```

## Limitações conhecidas

### DFA não minimizado

O DFA gerado pela conversão (NFA → DFA) utiliza a **construção de subconjuntos**, que garante apenas que todos os estados são alcançáveis. O DFA resultante **não é minimizado** — estados equivalentes não são fundidos, podendo haver estados redundantes.

Isso não afeta o teste de aceitação de strings, mas o DFA exibido pode conter mais estados que o mínimo teórico.

### Símbolos numéricos no YAML

O parser YAML (feito com IA) trata valores numéricos não-quotados de forma incorreta. Exemplo:

```yaml
# ERRADO: 0 é interpretado como número, vira "0.0" no autômato
alphabet: [0, 1]

# CERTO: use aspas para garantir que sejam lidos como string
alphabet: ['0', '1']
```

Isso acontece porque a biblioteca `aeson` converte números do YAML para o tipo `Scientific`, e `show` de um científico sempre produz `"0.0"` em vez de `"0"`. A solução é **sempre usar aspas** ao redor dos símbolos no arquivo YAML.
