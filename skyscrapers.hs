-- Wolkenkratzer (Skyscrapers) solver
-- https://www.janko.at/Raetsel/Wolkenkratzer/index.htm
--
-- Regras do puzzle:
--   * Numa grade n x n, cada célula recebe uma altura de 1 a n.
--   * Cada altura aparece exatamente uma vez em cada linha e em cada coluna.
--   * Um número na borda diz quantos prédios são vistos olhando para dentro
--     da grade a partir daquela posição. Um prédio esconde todos os mais
--     baixos que estão atrás dele.
--   * Algumas pistas podem estar ausentes e algumas células podem já vir
--     preenchidas no enunciado.
--
-- Estratégia escolhida (backtracking):
--   1. Para cada linha, calculamos de antemão todas as ordenações de 1..n
--      que satisfazem as pistas da esquerda e da direita daquela linha
--      (e as células já dadas). Essas são as candidatas da linha.
--   2. Fazemos o mesmo para cada coluna, com as pistas de cima e de baixo.
--   3. Colocamos uma linha por vez, de cima para baixo. Depois de colocar
--      uma linha, verificamos se cada coluna parcial ainda é o começo de
--      alguma coluna válida. Se não é, a linha é rejeitada e tentamos a
--      próxima candidata. Se nenhuma candidata serve, voltamos uma linha
--      (backtracking).
--   4. Quando todas as linhas foram colocadas, a grade é uma solução.
--
-- Como nada é modificado (as listas são imutáveis), "voltar uma linha" não
-- exige desfazer nada: a função que chamou ainda tem a grade anterior.

module Main where

import Data.List ((\\))

------------------------------------------------------------------------
-- Modelagem do tabuleiro
------------------------------------------------------------------------

-- uma grade é uma lista de linhas; cada linha é uma lista de alturas.
-- quando a grade representa as células dadas no enunciado, 0 significa
-- "célula vazia".
type Grid = [[Int]]

-- as pistas nas bordas, nesta ordem: topo, base, esquerda, direita.
-- topo e base são lidas da esquerda para a direita; esquerda e direita são
-- lidas de cima para baixo. Uma pista igual a 0 significa "não informada"
data Clues = Clues [Int] [Int] [Int] [Int]

------------------------------------------------------------------------
-- Programa principal
------------------------------------------------------------------------

-- resolve os tabuleiros de exemplo e confere as respostas
main :: IO ()
main = do
  run "janko.at 5x5 (12 de 20 pistas)" puzzle5 (emptyGrid 5) solution5
  run "janko.at Nr. 3, 6x6 (todas as 24 pistas)" janko003 (emptyGrid 6) solution003
  run "janko.at Nr. 42, 6x6 (12 de 24 pistas, 2 celulas dadas)" janko042 givens042 solution042
  run "janko.at Nr. 43, 6x6 (12 de 24 pistas, 2 celulas dadas)" janko043 givens043 solution043

------------------------------------------------------------------------
-- Visibilidade
------------------------------------------------------------------------

-- quantos prédios são vistos olhando ao longo da lista, a partir do início.
-- um prédio é visto quando é mais alto do que todos os que vêm antes dele.
-- ex: visible [2,1,4,3] = 2 (o 2 e o 4); visible [4,3,2,1] = 1
visible :: [Int] -> Int
visible line = visibleAfter 0 line

-- igual à anterior, mas supondo que um prédio de altura `tallest` já está
-- na frente. Começamos com 0 para que o primeiro prédio seja sempre visto
visibleAfter :: Int -> [Int] -> Int
visibleAfter _ [] = 0
visibleAfter tallest (h : hs)
  | h > tallest = 1 + visibleAfter h hs -- mais alto que tudo antes: conta
  | otherwise = visibleAfter tallest hs -- escondido: não conta

-- uma linha satisfaz uma pista? Uma pista 0 (não informada) aceita tudo
-- a equação com 0 precisa vir primeiro, pois as equações são testadas em ordem
matches :: Int -> [Int] -> Bool
matches 0 _ = True
matches clue line = visible line == clue

------------------------------------------------------------------------
-- Linhas candidatas
------------------------------------------------------------------------

-- todas as ordenações (permutações) de uma lista
-- por ex, para [1,2,3] são 6; para [1..6] são 720
-- list comprehension: escolha um elemento x qualquer, depois qualquer
-- ordenação `rest` do que sobrou (xs \\ [x] remove uma ocorrência de x),
-- e coloque x na frente
-- O caso base é [[]] (uma única ordenação: a vazia), e não [] (nenhuma)
perms :: [Int] -> [[Int]]
perms [] = [[]]
perms xs = [x : rest | x <- xs, rest <- perms (xs \\ [x])]

-- todas as ordenações de 1..n que satisfazem a pista da frente e a pista
-- de trás. A pista de trás é testada sobre a lista invertida, porque é isso
-- que se vê entrando pelo outro lado
-- Serve tanto para linhas (esquerda, direita) quanto para colunas (topo, base)
lineOptions :: Int -> Int -> Int -> [[Int]]
lineOptions n front back =
  [p | p <- perms [1 .. n], matches front p, matches back (reverse p)]

-- uma linha candidata respeita as células dadas no enunciado quando, em
-- cada posição dada (diferente de 0), a altura coincide
fitsGivens :: [Int] -> [Int] -> Bool
fitsGivens [] _ = True
fitsGivens _ [] = True
fitsGivens (g : gs) (x : xs) = (g == 0 || g == x) && fitsGivens gs xs

------------------------------------------------------------------------
-- Colunas
------------------------------------------------------------------------

-- a coluna c das linhas já colocadas, de cima para baixo
column :: Grid -> Int -> [Int]
column rows c = [row !! c | row <- rows]

-- A primeira lista é o começo da segunda?
-- A lista vazia é começo de qualquer lista; nada não vazio é começo da
-- lista vazia; caso contrário compara as cabeças e continua
isPrefix :: [Int] -> [Int] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x : xs) (y : ys) = x == y && isPrefix xs ys

-- a coluna parcial ainda pode virar uma das colunas válidas?
-- basta existir uma opção que comece com o que já foi colocado.
-- quando a coluna está completa, "começar com" vira "ser igual a", ou seja,
-- a verificação passa a ser exata
stillPossible :: [[Int]] -> [Int] -> Bool
stillPossible options partial = [o | o <- options, isPrefix partial o] /= []

-- todos os valores da lista são True? (o && para na primeira False)
allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (b : bs) = b && allTrue bs

------------------------------------------------------------------------
-- Busca (backtracking)
------------------------------------------------------------------------

-- o primeiro Just de uma lista, ou Nothing se não houver nenhum
-- como as listas em haskell são lazy, os elementos depois do
-- primeiro Just nunca chegam a ser calculados: a busca para na primeira
-- solução encontrada sem precisar de uma forma explícita de parada
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : rest) = firstJust rest

-- resolve o puzzle: recebe as pistas e a grade de células dadas (0 = vazia)
-- e devolve Just grade se encontrou solução, ou Nothing se não existe
--
--   rowOpts: para cada linha, as ordenações permitidas pelas suas pistas
--            (esquerda e direita) e pelas células dadas naquela linha
--   colOpts: para cada coluna, as ordenações permitidas pelas suas pistas
--            (topo e base)
--   ambas são calculadas uma única vez (no `where` do solve) e
--   compartilhadas por todas as chamadas de `go`
--
--   go:        recebe as linhas já colocadas e as listas de candidatas das
--              linhas que faltam. Sem linhas faltando, a grade é solução
--              Caso contrário, tenta cada candidata da próxima linha e fica
--              com a primeira que leva a uma solução
--   tryRow:    uma tentativa. Se as colunas continuam viáveis, desce um
--              nível (go); senão devolve Nothing e firstJust passa para a
--              próxima candidata. É aqui que acontece o backtracking
--   columnsOk: cada coluna parcial precisa ser o começo de alguma coluna
--              válida.
solve :: Clues -> Grid -> Maybe Grid
solve (Clues top bottom left right) givens = go [] rowOpts
  where
    n = length top
    rowOpts = [rowOptions i | i <- [0 .. n - 1]]
    colOpts = [lineOptions n (top !! i) (bottom !! i) | i <- [0 .. n - 1]]

    -- candidatas da linha i: passam pelas pistas e pelas células dadas
    rowOptions :: Int -> [[Int]]
    rowOptions i =
      [p | p <- lineOptions n (left !! i) (right !! i), fitsGivens (givens !! i) p]

    go :: Grid -> [[[Int]]] -> Maybe Grid
    go rows [] = Just rows
    go rows (opts : rest) = firstJust [tryRow (rows ++ [p]) rest | p <- opts]

    tryRow :: Grid -> [[[Int]]] -> Maybe Grid
    tryRow rows rest
      | columnsOk rows = go rows rest
      | otherwise = Nothing

    columnsOk :: Grid -> Bool
    columnsOk rows =
      allTrue [stillPossible (colOpts !! c) (column rows c) | c <- [0 .. n - 1]]

------------------------------------------------------------------------
-- saída
------------------------------------------------------------------------

-- resolve um tabuleiro, printa o resultado e confere com a solução
run :: String -> Clues -> Grid -> Grid -> IO ()
run name clues givens expected = do
  putStrLn ("== " ++ name)
  printResult (solve clues givens) expected
  putStrLn ""

-- printa a matriz ou avisa que não há solução
printResult :: Maybe Grid -> Grid -> IO ()
printResult Nothing _ = putStrLn "sem solucao"
printResult (Just grid) expected = do
  printRows grid
  if grid == expected
    then putStrLn "confere com a solucao publicada"
    else putStrLn "DIFERENTE da solucao publicada"

-- recursão sobre a lista de linhas dentro de IO. `return ()` 
-- não faz nada, mas é necessário pq toda equação precisa devolver um IO ()
printRows :: Grid -> IO ()
printRows [] = return ()
printRows (row : rows) = do
  print row
  printRows rows

------------------------------------------------------------------------
-- Tabuleiros de exemplo
------------------------------------------------------------------------

-- grade n x n sem nenhuma célula dada
emptyGrid :: Int -> Grid
emptyGrid n = [[0 | _ <- [1 .. n]] | _ <- [1 .. n]]

-- 5x5 do site, 12 das 20 pistas informadas
puzzle5 :: Clues
puzzle5 = Clues [0, 3, 3, 0, 0] [0, 2, 2, 2, 1] [1, 4, 3, 2, 0] [3, 2, 3, 0, 1]

solution5 :: Grid
solution5 =
  [ [5, 3, 1, 4, 2],
    [1, 2, 3, 5, 4],
    [3, 4, 5, 2, 1],
    [4, 5, 2, 1, 3],
    [2, 1, 4, 3, 5]
  ]

-- tabuleiro número 3 retirado do site. 6x6, todas as 24 pistas
janko003 :: Clues
janko003 = Clues [4, 1, 2, 2, 3, 2] [1, 3, 5, 2, 4, 2] [2, 3, 3, 4, 2, 1] [2, 4, 2, 3, 1, 4]

solution003 :: Grid
solution003 =
  [ [2, 6, 3, 4, 1, 5],
    [4, 5, 6, 3, 2, 1],
    [3, 1, 5, 2, 6, 4],
    [1, 2, 4, 6, 5, 3],
    [5, 3, 2, 1, 4, 6],
    [6, 4, 1, 5, 3, 2]
  ]

-- tabuleiro número 42 retirado do site. 6x6, 12 das 24
-- pistas e duas células já preenchidas no enunciado
janko042 :: Clues
janko042 = Clues [3, 0, 0, 2, 2, 0] [0, 0, 2, 2, 0, 4] [3, 2, 0, 0, 0, 0] [4, 5, 1, 0, 0, 5]

givens042 :: Grid
givens042 =
  [ [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 5, 0, 0, 0, 0],
    [0, 0, 0, 0, 6, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0]
  ]

solution042 :: Grid
solution042 =
  [ [1, 2, 6, 5, 4, 3],
    [5, 6, 4, 3, 2, 1],
    [4, 5, 3, 2, 1, 6],
    [3, 4, 2, 1, 6, 5],
    [2, 3, 1, 6, 5, 4],
    [6, 1, 5, 4, 3, 2]
  ]

-- tabuleiro número 43 retirado do site. 6x6, 12 das 24
-- pistas e duas células já preenchidas no enunciado
janko043 :: Clues
janko043 = Clues [0, 0, 0, 0, 2, 3] [0, 0, 0, 4, 0, 1] [4, 1, 3, 4, 0, 0] [0, 4, 4, 2, 4, 0]

givens043 :: Grid
givens043 =
  [ [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 3, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0]
  ]

solution043 :: Grid
solution043 =
  [ [1, 2, 3, 6, 5, 4],
    [6, 1, 2, 5, 4, 3],
    [4, 5, 6, 3, 2, 1],
    [2, 3, 4, 1, 6, 5],
    [5, 6, 1, 4, 3, 2],
    [3, 4, 5, 2, 1, 6]
  ]
