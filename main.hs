-- Paula Rinco Rodrigues Pereira - 201865559C
-- Luciana Nascimento Santana Prachedes - 2020

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
    resultado <- realizaJogadas listaDeDados True  -- True significa que é a vez do jogador
    putStrLn resultado

-- Função para criar uma lista de Dados com valores aleatórios
criarListaDeDados :: Int -> IO [Dado]
criarListaDeDados n = sequence $ replicate n gerarDadoAleatorio

-- Função para gerar um dado com valor aleatório entre 1 e 6
gerarDadoAleatorio :: IO Dado
gerarDadoAleatorio = do
    valor <- randomRIO (1, 6)
    return (Dado valor)

-- Função recursiva para realizar jogadas alternadas e retornar o resultado
realizaJogadas :: [Dado] -> Bool -> IO String
realizaJogadas [] _ = return "Todos os dados foram removidos. Jogo encerrado."
realizaJogadas dados True = do
    putStrLn "É a sua vez de jogar."
    dadosAtualizados <- realizaJogada dados
    if null dadosAtualizados
        then return "Parabéns! Você venceu!"
        else realizaJogadas dadosAtualizados False  -- Alterna para a vez do computador
realizaJogadas dados False = do
    putStrLn "É a vez do computador."
    dadosAtualizados <- jogadaComputador dados
    if null dadosAtualizados
        then return "O computador venceu!"
        else realizaJogadas dadosAtualizados True  -- Alterna para a vez do jogador

-- Função para realizar uma jogada do jogador
realizaJogada :: [Dado] -> IO [Dado]
realizaJogada dados = do
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
                    return dadosAtualizados
                else do
                    putStrLn ("O dado escolhido tem o valor " ++ show valorEscolhido ++ ".")
                    putStrLn "Escolha o novo valor para o dado (menor que o valor atual):"
                    let valoresPossiveis = [1..(valorEscolhido - 1)]
                    putStrLn ("Valores possíveis: " ++ show valoresPossiveis)
                    novoValorInput <- getLine
                    let novoValor = read novoValorInput :: Int
                    if novoValor `elem` valoresPossiveis
                        then do
                            let dadoAtualizado = Dado novoValor
                            let dadosAtualizados = atualizarDado dados (escolha - 1) dadoAtualizado
                            putStrLn ("Dado atualizado de " ++ show dadoEscolhido ++ " para " ++ show dadoAtualizado)
                            putStrLn ("Lista de dados atualizada: " ++ show dadosAtualizados)
                            return dadosAtualizados
                        else do
                            putStrLn "Valor inválido. Tente novamente."
                            realizaJogada dados
        else do
            putStrLn "Escolha inválida. Tente novamente."
            realizaJogada dados

-- Função para a jogada do computador
jogadaComputador :: [Dado] -> IO [Dado]
jogadaComputador dados = do
    let indices = [0..length dados - 1]
    idx <- randomRIO (0, length dados - 1)
    let dadoEscolhido = dados !! idx
    let valorEscolhido = obterValor dadoEscolhido
    let valoresPossiveis = [1..(valorEscolhido - 1)]
    if null valoresPossiveis
        then do
            let dadosAtualizados = removeDado dados idx
            putStrLn ("Computador removeu o dado: " ++ show dadoEscolhido)
            putStrLn ("Lista de dados atualizada: " ++ show dadosAtualizados)
            return dadosAtualizados
        else do
            novoValor <- randomRIO (1, valorEscolhido - 1)  -- Escolhe um valor aleatório menor que o valor atual
            let dadoAtualizado = Dado novoValor
            let dadosAtualizados = atualizarDado dados idx dadoAtualizado
            putStrLn ("Computador atualizou o dado de " ++ show dadoEscolhido ++ " para " ++ show dadoAtualizado)
            putStrLn ("Lista de dados atualizada: " ++ show dadosAtualizados)
            return dadosAtualizados

-- Função para obter o valor de um Dado
obterValor :: Dado -> Int
obterValor (Dado x) = x

-- Função para remover um dado da lista com base no índice
removeDado :: [Dado] -> Int -> [Dado]
removeDado dados idx = take idx dados ++ drop (idx + 1) dados

-- Função para atualizar um dado na lista com base no índice
atualizarDado :: [Dado] -> Int -> Dado -> [Dado]
atualizarDado dados idx novoDado = take idx dados ++ [novoDado] ++ drop (idx + 1) dados
