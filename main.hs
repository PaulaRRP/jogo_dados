import System.Random (randomRIO)
import Data.Maybe (fromMaybe)

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
    putStrLn "Escolha o número do dado a ser removido (1 a N):"
    let listaIndices = zip [1..] dados
    mapM_ (\(i, dado) -> putStrLn (show i ++ ": " ++ show dado)) listaIndices
    input <- getLine
    let escolha = read input :: Int
    if escolha >= 1 && escolha <= length dados
        then do
            let dadoEscolhido = dados !! (escolha - 1)
            let dadosAtualizados = removeDado dados (escolha - 1)
            putStrLn ("Dado removido: " ++ show dadoEscolhido)
            putStrLn ("Lista de dados atualizada: " ++ show dadosAtualizados)
            realizaJogadas dadosAtualizados
        else do
            putStrLn "Escolha inválida. Tente novamente."
            realizaJogadas dados

-- Função para remover um dado da lista com base no índice
removeDado :: [Dado] -> Int -> [Dado]
removeDado dados idx = take idx dados ++ drop (idx + 1) dados
