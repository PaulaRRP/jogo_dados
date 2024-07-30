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

-- Função para criar uma lista de Dados com valores aleatórios
criarListaDeDados :: Int -> IO [Dado]
criarListaDeDados n = sequence $ replicate n gerarDadoAleatorio

-- Função para gerar um dado com valor aleatório entre 1 e 6
gerarDadoAleatorio :: IO Dado
gerarDadoAleatorio = do
    valor <- randomRIO (1, 6)
    return (Dado valor)
