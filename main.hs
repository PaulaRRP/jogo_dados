import System.Random (randomRIO)

-- Define um novo tipo Dado, que é um wrapper para Int
newtype Dado = Dado Int deriving (Show)

-- Função principal do programa
main :: IO ()
main = do
    putStrLn "Digite o número de dados:"
    input <- getLine
    let numDados = read input :: Int
    listaDeDados <- criarListaDeDados numDados
    putStrLn ("Seu jogo será iniciado com " ++ show (length listaDeDados) ++ " dados.")
    putStrLn ("Lista de dados: " ++ show listaDeDados)
    realizaJogadas listaDeDados

-- Função para criar uma lista de Dados com valores aleatórios
criarListaDeDados :: Int -> IO [Dado]
criarListaDeDados n = sequence $ replicate n gerarDadoAleatorio

-- Função para gerar um dado com valor aleatório entre 1 e 6
gerarDadoAleatorio :: IO Dado
gerarDadoAleatorio = do
    valor <- randomRIO (1, 6)
    return (Dado valor)

-- Função recursiva para realizar jogadas
realizaJogadas :: [Dado] -> IO ()
realizaJogadas [] = putStrLn "Todos os dados foram removidos. Jogo encerrado."
realizaJogadas dados = do
    putStrLn "Escolha o número do dado a ser removido ou girado (1 a N):"
    let listaIndices = zip [1..] dados
    mapM_ (\(i, dado) -> putStrLn (show i ++ ": " ++ show dado)) listaIndices
    input <- getLine
    let escolha = read input :: Int
    if escolha >= 1 && escolha <= length dados
        then do
            let dadoEscolhido = dados !! (escolha - 1)
            let valorEscolhido = obterValor dadoEscolhido
            if valorEscolhido == 1
                then do
                    let dadosAtualizados = removeDado dados (escolha - 1)
                    putStrLn ("Dado removido: " ++ show dadoEscolhido)
                    putStrLn ("Lista de dados atualizada: " ++ show dadosAtualizados)
                    realizaJogadas dadosAtualizados
                else do
                    putStrLn ("O dado escolhido tem o valor " ++ show valorEscolhido ++ ".")
                    putStrLn "Escolha o novo valor para o dado (exceto o valor oposto):"
                    let opostos = oposto valorEscolhido
                    putStrLn ("Valores possíveis: " ++ show (filtrarOpostos [1, 2, 3, 4, 5, 6] opostos))
                    novoValorInput <- getLine
                    let novoValor = read novoValorInput :: Int
                    if novoValor `elem` filtrarOpostos [1, 2, 3, 4, 5, 6] opostos
                        then do
                            let dadoAtualizado = Dado novoValor
                            let dadosAtualizados = atualizarDado dados (escolha - 1) dadoAtualizado
                            putStrLn ("Dado atualizado de " ++ show dadoEscolhido ++ " para " ++ show dadoAtualizado)
                            putStrLn ("Lista de dados atualizada: " ++ show dadosAtualizados)
                            realizaJogadas dadosAtualizados
                        else do
                            putStrLn "Valor inválido. Tente novamente."
                            realizaJogadas dados
        else do
            putStrLn "Escolha inválida. Tente novamente."
            realizaJogadas dados

-- Função para obter o valor de um Dado
obterValor :: Dado -> Int
obterValor (Dado x) = x

-- Função para remover um dado da lista com base no índice
removeDado :: [Dado] -> Int -> [Dado]
removeDado dados idx = take idx dados ++ drop (idx + 1) dados

-- Função para atualizar um dado na lista com base no índice
atualizarDado :: [Dado] -> Int -> Dado -> [Dado]
atualizarDado dados idx novoDado = take idx dados ++ [novoDado] ++ drop (idx + 1) dados

-- Função para determinar o valor oposto no dado
oposto :: Int -> Int
oposto x = case x of
    1 -> 6
    2 -> 5
    3 -> 4
    4 -> 3
    5 -> 2
    6 -> 1
    _ -> error "Valor do dado inválido"

-- Função para filtrar os valores possíveis, excluindo o oposto
filtrarOpostos :: [Int] -> Int -> [Int]
filtrarOpostos valores oposto = filter (/= oposto) valores
