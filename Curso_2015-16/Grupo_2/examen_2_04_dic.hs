-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (4 de diciembre de 2015)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    inserta :: [a] -> [[a]] -> [[a]]
-- tal que (inserta xs yss) es la lista obtenida insertando 
-- + el primer elemento de xs como primero en la primera lista de yss,
-- + el segundo elemento de xs como segundo en la segunda lista de yss
--   (si la segunda lista de yss tiene al menos un elemento), 
-- + el tercer elemento de xs como tercero en la tercera lista de yss
--   (si la tercera lista de yss tiene al menos dos elementos), 
-- y así sucesivamente. Por ejemplo, 
--    inserta [1,2,3] [[4,7],[6],[9,5,8]]  ==  [[1,4,7],[6,2],[9,5,3,8]]
--    inserta [1,2,3] [[4,7],[] ,[9,5,8]]  ==  [[1,4,7],[],   [9,5,3,8]]
--    inserta [1,2]   [[4,7],[6],[9,5,8]]  ==  [[1,4,7],[6,2],[9,5,8]]
--    inserta [1,2,3] [[4,7],[6]]          ==  [[1,4,7],[6,2]]
--    inserta "tad"   ["odo","pra","naa"]  ==  ["todo","para","nada"]
-- ---------------------------------------------------------------------

inserta :: [a] -> [[a]] -> [[a]]
inserta xs yss = aux xs yss 0 where
    aux [] yss _ = yss
    aux xs []  _ = []
    aux (x:xs) (ys:yss) n 
        | length us == n = (us ++ x : vs) : aux xs yss (n+1)
        | otherwise      = ys : aux xs yss (n+1)
        where (us,vs) = splitAt n ys

-- ---------------------------------------------------------------------
-- Ejercicio 2. El siguiente tipo de dato representa expresiones
-- construidas con variables, sumas y productos
--    data Expr = Var String
--              | S Expr Expr
--              | P Expr Expre
--              deriving (Eq, Show)
-- Por ejemplo, x*(y+z) se representa por (P (V "x") (S (V "y") (V "z"))) 
-- 
-- Una expresión es un término si es un producto de variables. Por
-- ejemplo, x*(y*z) es un término pero x+(y*z) ni x*(y+z) lo son.
--
-- Una expresión está en forma normal si es una suma de términos. Por
-- ejemplo, x*(y*z) y x+(y*z) está en forma normal; pero x*(y+z) y
-- (x+y)*(x+z) no lo están. 
-- 
-- Definir la función 
--    normal :: Expr -> Expr
-- tal que (normal e) es la forma normal de la expresión e obtenida
-- aplicando, mientras que sea posible, las propiedades distributivas:
--    (a+b)*c = a*c+b*c
--    c*(a+b) = c*a+c*b
-- Por ejemplo,
--    ghci> normal (P (S (V "x") (V "y")) (V "z"))
--    S (P (V "x") (V "z")) (P (V "y") (V "z"))
--    ghci> normal (P (V "z") (S (V "x") (V "y")))
--    S (P (V "z") (V "x")) (P (V "z") (V "y"))
--    ghci> normal (P (S (V "x") (V "y")) (S (V "u") (V "v")))
--    S (S (P (V "x") (V "u")) (P (V "x") (V "v"))) 
--      (S (P (V "y") (V "u")) (P (V "y") (V "v")))
--    ghci> normal (S (P (V "x") (V "y")) (V "z"))
--    S (P (V "x") (V "y")) (V "z")
--    ghci> normal (V "x")
--    V "x"
-- ---------------------------------------------------------------------

data Expr = V String
          | S Expr Expr
          | P Expr Expr
          deriving (Eq, Show)

esTermino :: Expr -> Bool
esTermino (V _)   = True
esTermino (S _ _) = False
esTermino (P a b) = esTermino a && esTermino b

esNormal :: Expr -> Bool
esNormal (S a b) = esNormal a && esNormal b
esNormal a       = esTermino a

normal :: Expr -> Expr
normal (V v)   = V v
normal (S a b) = S (normal a) (normal b)
normal (P a b) = p (normal a) (normal b)
    where p (S a b) c = S (p a c) (p b c)
          p a (S b c) = S (p a b) (p a c)
          p a b       = P a b

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Un elemento de una lista es punta si ninguno de los
-- siguientes es mayor que él.  
-- 
-- Definir la función 
--    puntas :: [Int] -> [Int]
-- tal que (puntas xs) es la lista de los elementos puntas de xs. Por
-- ejemplo, 
--    puntas [80,1,7,8,4]  ==  [80,8,4]
-- ---------------------------------------------------------------------

-- 1ª definición:
puntas1 :: [Int] -> [Int]
puntas1 [] = []
puntas1 (x:xs) | x == maximum (x:xs) = x : puntas1 xs
               | otherwise           = puntas1 xs

-- 2ª definición (sin usar maximum):
puntas2 :: [Int] -> [Int]
puntas2 [] = []
puntas2 (x:xs) | all (<=x) xs = x : puntas2 xs
               | otherwise    = puntas2 xs

-- 3ª definición (por plegado):
puntas3 :: [Int] -> [Int]
puntas3 = foldr f []
    where f x ys | all (<= x) ys = x:ys
                 | otherwise     = ys

-- 4ª definición (por plegado y acumulador):
puntas4 :: [Int] -> [Int]
puntas4 xs = foldl f [] (reverse xs)
    where f ac x | all (<=x) ac = x:ac
                 | otherwise    = ac

-- Nota: Comparación de eficiencia
--    ghci> let xs = [1..4000] in last (puntas1 (xs ++ reverse xs))
--    1
--    (3.58 secs, 2,274,856,232 bytes)
--    ghci> let xs = [1..4000] in last (puntas2 (xs ++ reverse xs))
--    1
--    (2.27 secs, 513,654,880 bytes)
--    ghci> let xs = [1..4000] in last (puntas3 (xs ++ reverse xs))
--    1
--    (2.54 secs, 523,669,416 bytes)
--    ghci> let xs = [1..4000] in last (puntas4 (xs ++ reverse xs))
--    1
--    (2.48 secs, 512,952,728 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Consideremos el tipo de los árboles binarios con enteros
-- en las hojas y nodos, definido por:
--    data Arbol1 = H1 Int
--                | N1 Int Arbol1 Arbol
--                deriving (Eq, Show)
-- y el tipo de árbol binario de hojas vacías y enteros en los nodos,
-- definido por
--    data Arbol2 = H2
--                | N2 Int Arbol2 Arbol2
--                deriving (Eq, Show)
-- 
-- Por ejemplo, los árboles
--         5                           10 
--        / \                         / \
--       /   \                       /   \
--      3     7                     5     15
--     / \   / \                   /\     /\
--    1   4 6   9                 .  .   .  .
-- se definen por
--    ejArbol1 :: Arbol1
--    ejArbol1 = N1 5 (N1 3 (H1 1) (H1 4)) (N1 7 (H1 6) (H1 9))
--    ejArbol2 :: Arbol2
--    ejArbol2 = N2 10 (N2 5 H2 H2) (N2 15 H2 H2)
-- 
-- Definir la función
--    comprime :: Arbol1 -> Arbol2
-- tal que (comprime a) es el árbol obtenido sustituyendo cada nodo por
-- la suma de sus hijos. Por ejemplo,
--    ghci> comprime ejArbol1
--    N2 10 (N2 5 H2 H2) (N2 15 H2 H2)
--    ghci> comprime ejArbol1 == ejArbol2
--    True
-- ---------------------------------------------------------------------

data Arbol1 = H1 Int
            | N1 Int Arbol1 Arbol1
            deriving (Eq, Show)

data Arbol2 = H2
            | N2 Int Arbol2 Arbol2
            deriving (Eq, Show)

ejArbol1 :: Arbol1
ejArbol1 = N1 5 (N1 3 (H1 1) (H1 4)) (N1 7 (H1 6) (H1 9))

ejArbol2 :: Arbol2
ejArbol2 = N2 10 (N2 5 H2 H2) (N2 15 H2 H2)

comprime :: Arbol1 -> Arbol2
comprime (H1 x)     = H2 
comprime (N1 x i d) = N2 (raiz i + raiz d) (comprime i) (comprime  d)

raiz :: Arbol1 -> Int
raiz (H1 x)     = x
raiz (N1 x _ _) = x
