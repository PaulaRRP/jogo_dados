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
    modo <- escolherModo
    resultado <- if modo == 2
                    then realizaJogadasDificil listaDeDados False -- Nível difícil, computador começa
                    else realizaJogadas listaDeDados True         -- Nível fácil, jogador começa
    putStrLn resultado

-- Função para criar uma lista de Dados com valores aleatórios
criarListaDeDados :: Int -> IO [Dado]
criarListaDeDados n = sequence $ replicate n gerarDadoAleatorio

-- Função para gerar um dado com valor aleatório entre 1 e 6
gerarDadoAleatorio :: IO Dado
gerarDadoAleatorio = do
    valor <- randomRIO (1, 6)
    return (Dado valor)

-- Função para verificar e escolher o modo de jogo
escolherModo :: IO Int
escolherModo = do
    putStrLn "Escolha o modo de jogo: 1 para fácil, 2 para difícil"
    modoInput <- getLine
    let modo = readModo modoInput
    case modo of
        Nothing -> do
            putStrLn "Modo inválido. Tente novamente."
            escolherModo
        Just m  -> return m
  where
    readModo :: String -> Maybe Int
    readModo str = case reads str of
        [(n, "")] | n `elem` [1, 2] -> Just n
        _ -> Nothing

-- Função recursiva para realizar jogadas alternadas e retornar o resultado para nível fácil
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
        
-- Função recursiva para realizar jogadas alternadas e retornar o resultado para nível difícil
realizaJogadasDificil :: [Dado] -> Bool -> IO String
realizaJogadasDificil [] _ = return "Todos os dados foram removidos. Jogo encerrado."
realizaJogadasDificil dados True = do
    putStrLn "É a sua vez de jogar."
    dadosAtualizados <- realizaJogada dados
    if null dadosAtualizados
        then return "Parabéns! Você venceu!"
        else realizaJogadasDificil dadosAtualizados False  -- Alterna para a vez do computador
realizaJogadasDificil dados False = do
    putStrLn "É a vez do computador."
    dadosAtualizados <- jogadaComputadorDificil dados
    if null dadosAtualizados
        then return "O computador venceu!"
        else realizaJogadasDificil dadosAtualizados True  -- Alterna para a vez

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

-- Funções auxiliares para a análise da configuração

-- Função para verificar se uma configuração de um único dado é vencedora
configuracaoVencedora1 :: Int -> Bool
configuracaoVencedora1 1 = True
configuracaoVencedora1 2 = False
configuracaoVencedora1 3 = True
configuracaoVencedora1 4 = True
configuracaoVencedora1 5 = False
configuracaoVencedora1 6 = True
configuracaoVencedora1 _ = False

-- Função para verificar se uma configuração de dois dados é vencedora
configuracaoVencedora2 :: Int -> Int -> Bool
configuracaoVencedora2 x y
    | x == y = False
    | x + y == 7 = False
    | otherwise = True

-- Função para verificar se a configuração é vencedora ou perdedora
configuracaoVencedoraN :: [Int] -> Bool
configuracaoVencedoraN dados = 
    case filteredDados of
        [] -> True  -- Nenhum dado restante, configuração vencedora
        [x] -> configuracaoVencedora1 x  -- Verifica configuração de um único dado
        [x, y] -> configuracaoVencedora2 x y  -- Verifica configuração de dois dados
        _ -> not $ any (== False) validPairs  -- Verifica pares restantes
  where
    -- Filtra dados removendo faces 2 ou 5
    filteredDados = filter (`notElem` [2, 5]) dados
    
    -- Gera todos os pares de dados restantes
    pares = [(x, y) | x <- filteredDados, y <- filteredDados, x /= y]
    
    -- Filtra pares com faces iguais e pares que somam 7
    paresValidos = filter (\(x, y) -> x /= y && x + y /= 7) pares
    
    -- Verifica se todos os pares válidos são perdedores
    validPairs = map (uncurry configuracaoVencedora2) paresValidos

-- Função para a jogada do computador no nível difícil
jogadaComputadorDificil :: [Dado] -> IO [Dado]
jogadaComputadorDificil dados = do
    -- Encontrar dados que podem ser removidos (face 1)
    let dadosRemoviveis = [idx | (idx, dado) <- zip [0..] dados, obterValor dado == 1]
    let todasRotacoes = [atualizarDado dados idx (Dado novoValor) | idx <- [0..length dados - 1], novoValor <- [1..(obterValor (dados !! idx) - 1)]]

    -- Filtrar as jogadas que levam a uma configuração perdedora
    let jogadasRemocaoPerdedoras = [removeDado dados idx | idx <- dadosRemoviveis, not (configuracaoVencedoraN (map obterValor (removeDado dados idx)))]
    let rotacoesPerdedoras = filter (not . configuracaoVencedoraN . map obterValor) todasRotacoes
    
    if not (null jogadasRemocaoPerdedoras)
        then do
            -- Se houver jogadas de remoção que são perdedoras, escolha uma aleatoriamente
            idx <- randomRIO (0, length jogadasRemocaoPerdedoras - 1)
            let dadosAtualizados = jogadasRemocaoPerdedoras !! idx
            putStrLn ("Computador removeu um dado. Lista de dados atualizada: " ++ show dadosAtualizados)
            return dadosAtualizados
        else if not (null rotacoesPerdedoras)
            then do
                -- Se houver rotações que são perdedoras, escolha uma aleatoriamente
                idx <- randomRIO (0, length rotacoesPerdedoras - 1)
                let dadosAtualizados = rotacoesPerdedoras !! idx
                putStrLn ("Computador rotacionou um dado. Lista de dados atualizada: " ++ show dadosAtualizados)
                return dadosAtualizados
            else do
                -- Caso não encontre jogadas ou rotações perdedoras, faça uma jogada aleatória
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
                        novoValor <- randomRIO (1, valorEscolhido - 1)
                        let dadoAtualizado = Dado novoValor
                        let dadosAtualizados = atualizarDado dados idx dadoAtualizado
                        putStrLn ("Computador atualizou o dado de " ++ show dadoEscolhido ++ " para " ++ show dadoAtualizado)
                        putStrLn ("Lista de dados atualizada: " ++ show dadosAtualizados)
                        return dadosAtualizados
