-- Informática (1º del Grado en Matemáticas)
-- 6º examen de evaluación continua (18 de junio de 2014)
-- ---------------------------------------------------------------------

-- Librerías auxiliares                                             
-- ====================
import Data.List
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1 [2 puntos]. Definir la función 
--     siembra :: [a] -> [[a]] -> [[a]]
-- tal que (siembra xs yss) es la lista obtenida introduciendo cada uno
-- de los elementos de xs en la lista correspondiente de yss; es decir,
-- el primer elemento de xs en la primera lista de yss, el segundo
-- elemento de xs en la segunda lista de yss, etc. Por ejemplo,
--    siembra [1,2,3] [[4,7],[6],[9,5,8]]  ==  [[1,4,7],[2,6],[3,9,5,8]]
--    siembra [1,2] [[4,7],[6],[9,5,8]]    ==  [[1,4,7],[2,6],[9,5,8]]
--    siembra [1,2,3] [[4,7],[6]]          ==  [[1,4,7],[2,6]]
-- ---------------------------------------------------------------------

siembra :: [a] -> [[a]] -> [[a]]
siembra [] yss          = yss
siembra xs []           = []
siembra (x:xs) (ys:yss) = (x:ys) : siembra xs yss

-- ---------------------------------------------------------------------
-- Ejercicio 2 [2 puntos]. Definir la función 
--    primosEquidistantes :: Integer -> [(Integer,Integer)]
-- tal que (primosEquidistantes k) es la lista de los pares de primos
-- consecutivos cuya diferencia es k. Por ejemplo,
--    take 3 (primosEquidistantes 2)  ==  [(3,5),(5,7),(11,13)]
--    take 3 (primosEquidistantes 4)  ==  [(7,11),(13,17),(19,23)]
--    take 3 (primosEquidistantes 6)  ==  [(23,29),(31,37),(47,53)]
--    take 3 (primosEquidistantes 8)  ==  [(89,97),(359,367),(389,397)]
-- ---------------------------------------------------------------------

primosEquidistantes :: Integer -> [(Integer,Integer)]
primosEquidistantes k = aux primos
    where aux (x:y:ps) | y - x == k = (x,y) : aux (y:ps)
                       | otherwise  = aux (y:ps)
 
-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 8  ==  False
primo :: Integer -> Bool
primo x = [y | y <- [1..x], x `rem` y == 0] == [1,x]
 
-- primos es la lista de los números primos. Por ejemplo,
--    take 10 primos  ==  [2,3,5,7,11,13,17,19,23,29]
primos :: [Integer]
primos = 2 : [x | x <- [3,5..], primo x]

-- ---------------------------------------------------------------------
-- Ejercicio 3 [2 puntos]. Se consideran los árboles con operaciones
-- booleanas definidos por  
--    data ArbolB = H Bool 
--                | Conj ArbolB ArbolB
--                | Disy ArbolB ArbolB
--                | Neg ArbolB
-- 
-- Por ejemplo, los árboles
--                Conj                            Conj          
--               /   \                           /   \          
--              /     \                         /     \         
--           Disy      Conj                  Disy      Conj     
--          /   \       /  \                /   \      /   \    
--       Conj    Neg   Neg True          Conj    Neg   Neg  True 
--       /  \    |     |                 /  \    |     |        
--    True False False False          True False True  False     
--
-- se definen por
--    ej1, ej2:: ArbolB
--    ej1 = Conj (Disy (Conj (H True) (H False))
--                     (Neg (H False)))
--               (Conj (Neg (H False))
--                     (H True))
--    
--    ej2 = Conj (Disy (Conj (H True) (H False))
--                     (Neg (H True)))
--               (Conj (Neg (H False))
--                     (H True))
-- 
-- Definir la función 
--    valor:: ArbolB -> Bool
-- tal que (valor ar) es el resultado de procesar el árbol realizando
-- las operaciones booleanas especificadas en los nodos. Por ejemplo,
--    valor ej1 == True
--    valor ej2 == False
-- ---------------------------------------------------------------------

data ArbolB = H Bool 
            | Conj ArbolB ArbolB
            | Disy ArbolB ArbolB
            | Neg ArbolB

ej1, ej2:: ArbolB
ej1 = Conj (Disy (Conj (H True) (H False))
                 (Neg (H False)))
           (Conj (Neg (H False))
                 (H True))

ej2 = Conj (Disy (Conj (H True) (H False))
                 (Neg (H True)))
           (Conj (Neg (H False))
                 (H True))

valor:: ArbolB -> Bool
valor (H x)     = x
valor (Neg a)    = not (valor a)
valor (Conj i d) = (valor i) && (valor d)
valor (Disy i d) = (valor i) || (valor d)

-- ---------------------------------------------------------------------
-- Ejercicio 4 [2 puntos]. La matriz de Vandermonde generada por
-- [a(1),a(2),a(3),...,a(n)] es la siguiente
--    |1  a(1)  a(1)^2 ... a(1)^{n-1}|
--    |1  a(2)  a(2)^2 ... a(2)^{n-1}|
--    |1  a(3)  a(3)^2 ... a(3)^{n-1}|
--    |.  .     .          .         |
--    |.  .     .          .         |
--    |.  .     .          .         |
--    |1  a(n)  a(n)^2 ... a(n)^{n-1}|
--
-- Las matrices se representan con tablas cuyos índices son pares de
-- números naturales.  
--    type Matriz a = Array (Int,Int) a
--
-- Definir la función 
--    vandermonde:: [Integer] -> Matriz Integer
-- tal que (vandermonde xs) es la matriz de Vandermonde cuyos
-- generadores son los elementos de xs. Por ejemplo,
--    ghci> vandermonde [5,2,3,4]
--    array ((1,1),(4,4)) [((1,1),1),((1,2),5),((1,3),25),((1,4),125),
--                         ((2,1),1),((2,2),2),((2,3), 4),((2,4),  8),
--                         ((3,1),1),((3,2),3),((3,3), 9),((3,4), 27),
--                         ((4,1),1),((4,2),4),((4,3),16),((4,4), 64)]
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

-- 1ª solución
-- ===========

vandermonde1 :: [Integer] -> Matriz Integer
vandermonde1 xs = array ((1,1), (n,n)) 
                  [((i,j), f i j) | i <- [1..n], j <- [1..n]]
      where n     = length xs
            f i j = (xs!!(i-1))^(j-1)

-- 2ª solución
-- ===========

vandermonde2 :: [Integer] -> Matriz Integer
vandermonde2 xs = listArray ((1,1),(n,n)) (concat (listaVandermonde xs))
    where n = length xs

-- (listaVandermonde xs) es la lista correspondiente a la matriz de
-- Vandermonde generada por xs. Por ejemplo,
--    ghci> listaVandermonde [5,2,3,4]
--    [[1,5,25,125],[1,2,4,8],[1,3,9,27],[1,4,16,64]]
listaVandermonde :: [Integer] -> [[Integer]]
listaVandermonde xs = [[x^i | i <- [0..n-1]] | x <- xs]
    where n = length xs

-- ---------------------------------------------------------------------
-- Ejercicio 5 [2 puntos]. El número 595 es palíndromo y, además, es
-- suma de cuadrados consecutivos, pues 
--    595 = 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2. 
-- 
-- Definir la función 
--    sucesion:: [Integer]
-- tal que sucesion es la lista de los números que son palíndromos y
-- suma de cuadrados consecutivos. Por ejemplo,
--    take 10 sucesion == [1,4,5,9,55,77,121,181,313,434]
--    take 15 sucesion == [1,4,5,9,55,77,121,181,313,434,484,505,545,595,636]
-- ---------------------------------------------------------------------

sucesion:: [Integer]
sucesion = [x | x <-[1..], palindromo x, esSumaCuadradosConsecutivos x]

palindromo :: Integer -> Bool
palindromo n = show n == reverse (show n)

sucSumaCuadradosDesde :: Integer -> [Integer]    
sucSumaCuadradosDesde k = scanl (\s n -> s + n^2) 0 [k..]


esSumaCuadradosConsecutivos n =
    or [pertenece n (sucSumaCuadradosDesde k) | k <- [1..m]]
    where pertenece x xs = elem x (takeWhile (<=x) xs)
          m              = floor (sqrt (fromIntegral n))

-- 2ª solución para esSumaCuadradosConsecutivos:
        
esSumaCuadradosConsecutivos2 n = any (==n) (map sum yss)
    where m = floor (sqrt (fromIntegral n))
          xss = segmentos [1..m]
          yss = map (map (^2)) xss

segmentos :: [a] -> [[a]]
segmentos xs = concat [tail (inits ys) | ys <- init (tails xs)]
  
sucesion2:: [Integer]
sucesion2 = [x | x <-[1..], palindromo x, esSumaCuadradosConsecutivos2 x]
