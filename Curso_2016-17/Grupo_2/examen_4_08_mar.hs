-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (8 de marzo de 2017)    
-- =====================================================================

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.Array
import qualified Data.Set as S

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un número paritario es aquel que tiene una cantidad par
-- de cifras pares y una cantidad impar de cifras impares. Por ejemplo,
-- 111, 290, 11690, 29451 y 1109871 son paritarios, mientras que 21,
-- 2468 y 11358 no lo son.
--
-- Definir la constante
--    numerosParitarios :: [Integer]
-- cuyo valor es la lista ordenada de todos los números paritarios. Por
-- ejemplo,
--    take 10 numerosParitarios  ==  [1,3,5,7,9,100,102,104,106,108]
--    numerosParitarios !! 10     ==  111
--    numerosParitarios !! 100    ==  290
--    numerosParitarios !! 1000   ==  11090
--    numerosParitarios !! 10000  ==  29091
-- ---------------------------------------------------------------------

numerosParitarios :: [Integer]
numerosParitarios =
  filter numeroParitario [1..]

numeroParitario :: Integer -> Bool
numeroParitario n =
  even (length ps) && odd (length is)
  where ns = show n
        ps = filter (`elem` "02468") ns
        is = filter (`elem` "13579") ns

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    subconjuntosSinConsecutivos :: [Int] -> [[Int]]
-- tal que (subconjuntosSinConsecutivos xs) es la lista de los
-- subconjuntos de la lista xs en los que no haya números consecutivos.
-- Por ejemplo,
--    subconjuntosSinConsecutivos [2..4]   ==  [[2,4],[2],[3],[4],[]]
--    subconjuntosSinConsecutivos [2,4,3]  ==  [[2,4],[2],[4],[3],[]]
--    subconjuntosSinConsecutivos [1..5]   ==
--       [[1,3,5],[1,3],[1,4],[1,5],[1],[2,4],[2,5],[2],[3,5],[3],[4],[5],[]]
--    subconjuntosSinConsecutivos [5,2,4,1,3]  ==
--       [[5,2],[5,1,3],[5,1],[5,3],[5],[2,4],[2],[4,1],[4],[1,3],[1],[3],[]]
-- ---------------------------------------------------------------------

subconjuntosSinConsecutivos :: [Int] -> [[Int]]
subconjuntosSinConsecutivos [] = [[]]
subconjuntosSinConsecutivos (x:xs) =
  [x:ys | ys <- yss  
        , x-1 `notElem` ys
        , x+1 `notElem`  ys]
  ++ yss
  where yss = subconjuntosSinConsecutivos xs

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    matrizBloque :: Int -> Int -> Int -> Int -> Matriz a -> Matriz a
-- tal que (matrizBloque i1 i2 j1 j2 m) es la submatriz de la matriz m
-- formada por todos los elementos en las filas desde la i1 hasta la i2 
-- y en las columnas desde la j1 hasta la j2. Por ejemplo, si
--    m1 = array ((1,1),(9,9)) [((i,j),i*10+j) | i <- [1..9], j <- [1..9]]
-- entonces
--    matrizBloque 3 6 7 9 m1  ==
--       array ((1,1),(4,3)) [((1,1),37),((1,2),38),((1,3),39),
--                            ((2,1),47),((2,2),48),((2,3),49),
--                            ((3,1),57),((3,2),58),((3,3),59),
--                            ((4,1),67),((4,2),68),((4,3),69)]
--    matrizBloque 2 7 3 6 m1  ==
--       array ((1,1),(6,4)) [((1,1),23),((1,2),24),((1,3),25),((1,4),26),
--                            ((2,1),33),((2,2),34),((2,3),35),((2,4),36),
--                            ((3,1),43),((3,2),44),((3,3),45),((3,4),46),
--                            ((4,1),53),((4,2),54),((4,3),55),((4,4),56),
--                            ((5,1),63),((5,2),64),((5,3),65),((5,4),66),
--                            ((6,1),73),((6,2),74),((6,3),75),((6,4),76)]
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

m1 :: Matriz Int
m1 = array ((1,1),(9,9)) [((i,j),i*10+j) | i <- [1..9], j <- [1..9]]

matrizBloque :: Int -> Int -> Int -> Int -> Matriz a -> Matriz a
matrizBloque i1 i2 j1 j2 m 
  | 1 <= i1 && i1 <= i2 && i2 <= p &&
    1 <= j1 && j1 <= j2 && j2 <= q =
      array ((1,1),(i2-i1+1,j2-j1+1))
            [((i,j),m!(i+i1-1,j+j1-1)) | i <- [1..i2-i1+1],
                                         j <- [1..j2-j1+1]]
  | otherwise = error "La operación no se puede realizar"
  where (_,(p,q)) = bounds m

-- ---------------------------------------------------------------------
-- Ejercicio 4. Se dice que una operador @ es interno en un conjunto A
-- si al aplicar @ sobre elementos de A se obtiene como resultado
-- otro elemento de A. Por ejemplo, la suma es un operador interno en el
-- conjunto de los números naturales pares. La clausura de un conjunto A
-- con respecto a un operador @ es el menor conjunto B tal que A está
-- contenido en B y el operador @ es interno en el conjunto B. Por
-- ejemplo, la clausura del conjunto {2} con respecto a la suma es el
-- conjunto de los números pares positivos:
--    {2, 4, 6, 8, ...} = {2*k | k <- [1..]}
--
-- Definir la función
--    clausuraOperador :: (Int -> Int -> Int) -> S.Set Int -> S.Set Int
-- tal que (clausuraOperador op xs) es la clausura del conjunto xs con
-- respecto a la operación op. Por ejemplo,
--    clausuraOperador gcd (S.fromList [6,9,10])     ==
--       fromList [1,2,3,6,9,10]
--    clausuraOperador gcd (S.fromList [42,70,105])  ==
--       fromList [7,14,21,35,42,70,105]
--    clausuraOperador lcm (S.fromList [6,9,10])     ==
--       fromList [6,9,10,18,30,90]
--    clausuraOperador lcm (S.fromList [2,3,5,7])    ==
--       fromList [2,3,5,6,7,10,14,15,21,30,35,42,70,105,210]
-- ---------------------------------------------------------------------

clausuraOperador :: (Int -> Int -> Int) -> S.Set Int -> S.Set Int
clausuraOperador op =
  until (\ xs -> null [(x,y) | x <- S.elems xs,
                               y <- S.elems xs,
                               S.notMember (op x y) xs])
        (\ xs -> S.union xs (S.fromList [op x y | x <- S.elems xs,
                                                  y <- S.elems xs]))
