-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 4º examen de evaluación continua (12 de abril de 2010)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. En los apartados de este ejercicio se usará el tipo de
-- árboles binarios definidos como sigue  
--    data Arbol a = Hoja 
--                 | Nodo a (Arbol a) (Arbol a)
--                 deriving (Show, Eq)
-- En los ejemplos se usará el siguiente árbol
--    ejArbol :: Arbol Int
--    ejArbol = Nodo 2
--                   (Nodo 5
--                         (Nodo 3 Hoja Hoja) 
--                         (Nodo 7 Hoja Hoja)) 
--                   (Nodo 4 Hoja Hoja)
-- 
-- Definir por recursión la función 
--    sumaArbol :: Num a => Arbol a -> a
-- tal (sumaArbol x) es la suma de los valores que hay en el árbol
-- x. Por ejemplo,
--    sumaArbol ejArbol  ==  21
-- ---------------------------------------------------------------------

data Arbol a = Hoja 
             | Nodo a (Arbol a) (Arbol a)
             deriving (Show, Eq)

ejArbol :: Arbol Int
ejArbol = Nodo 2
               (Nodo 5
                     (Nodo 3 Hoja Hoja) 
                     (Nodo 7 Hoja Hoja)) 
               (Nodo 4 Hoja Hoja)

sumaArbol :: Num a => Arbol a -> a
sumaArbol Hoja = 0
sumaArbol (Nodo x i d) = x + sumaArbol i + sumaArbol d

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir por recursión la función
--    nodos :: Arbol a -> [a]
-- tal que (nodos x) es la lista de los nodos del árbol x. Por ejemplo.
--    nodos ejArbol  ==  [2,5,3,7,4]
-- ---------------------------------------------------------------------

nodos :: Arbol a -> [a]
nodos Hoja = []
nodos (Nodo x i d) = x : nodos i ++ nodos d

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Demostrar por inducción que para todo árbol a, 
-- sumaArbol a = sum (nodos a).
-- Indicar la propiedad de sum que se usa en la demostración.
-- ---------------------------------------------------------------------

{-
 Caso base: Hay que demostrar que
    sumaArbol Hoja = sum (nodos Hoja)
 En efecto,
    sumaArbol Hoja
    = 0                  [por sumaArbol.1]
    = sum []             [por suma.1]
    = sum (nodos Hoja)   [por nodos.1]

 Caso inductivo: Se supone la hipótesis de inducción
    sumaArbol i = sum (nodos i)
    sumaArbol d = sum (nodos d)
 Hay que demostrar que
    sumaArbol (Nodo x i d) = sum (nodos (Nodo x i d))
 En efecto,
    sumaArbol (Nodo x i d)
    = x + sumaArbol i + sumaArbol d       [por sumaArbol.2]
    = x + sum (nodos i) + sum (nodos d)   [por hip. de inducción]
    = x + sum (nodos i ++ nodos d)        [por propiedad de sum]
    = sum(x:(nodos i)++(nodos d))         [por sum.2]
    = sum (Nodos x i d)                   [por nodos.2]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la constante
--    pares :: Int
-- tal que pares es la lista de todos los pares de números enteros
-- positivos ordenada según la suma de sus componentes y el valor de la
-- primera componente. Por ejemplo, 
--    ghci> take 11 pares
--    [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),(4,1),(1,5)]
-- ---------------------------------------------------------------------

pares :: [(Integer,Integer)]
pares = [(x,z-x) | z <- [1..], x <- [1..z-1]]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la constante
--    paresDestacados :: [(Integer,Integer)]
-- tal que paresDestadados es la lista de pares de números enteros (x,y)
-- tales que 11 divide a x+13y y 13 divide a x+11y. 
-- ---------------------------------------------------------------------

paresDestacados :: [(Integer,Integer)]
paresDestacados = [(x,y) | (x,y) <- pares,
                           x+13*y `rem` 11 == 0,
                           x+11*y `rem` 13 == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir la constante
--    parDestacadoConMenorSuma :: Integer
-- tal que parDestacadoConMenorSuma es el par destacado con menor suma y
-- calcular su valor y su posición en la lista pares.
-- ---------------------------------------------------------------------

-- La definición es
parDestacadoConMenorSuma :: (Integer,Integer)
parDestacadoConMenorSuma = head paresDestacados

-- El valor es
--   ghci> parDestacadoConMenorSuma
--   (23,5)

-- La posición es
--    ghci> 1 + length (takeWhile (/=parDestacadoConMenorSuma) pares)
--    374

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    limite :: (Num a, Enum a, Num b, Ord b) => (a -> b) -> b -> b
-- tal que (limite f a) es el valor de f en el primer término x tal que 
-- para todo y entre x+1 y x+100, el valor absoluto de f(y)-f(x) es
-- menor que a. Por ejemplo,
--    limite (\n -> (2*n+1)/(n+5)) 0.001  ==  1.9900110987791344
--    limite (\n -> (1+1/n)**n) 0.001     ==  2.714072874546881
-- ---------------------------------------------------------------------

limite :: (Num a, Enum a, Num b, Ord b) => (a -> b) -> b -> b
limite f a = 
    head [f x | x <- [1..],
                maximum [abs(f y - f x) | y <- [x+1..x+100]] < a]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    esLimite :: (Num a, Enum a, Num b, Ord b) => 
--                (a -> b) -> b -> b -> Bool
-- tal que (esLimite f b a) se verifica si existe un x tal que para todo
-- y entre x+1 y x+100, el valor absoluto de f(y)-b es menor que a. Por
-- ejemplo, 
--    esLimite (\n -> (2*n+1)/(n+5)) 2 0.01     ==  True
--    esLimite (\n -> (1+1/n)**n) (exp 1) 0.01  ==  True
-- ---------------------------------------------------------------------

esLimite :: (Num a, Enum a, Num b, Ord b) => (a -> b) -> b -> b -> Bool
esLimite f b a =
    not (null [x | x <- [1..],
                   maximum [abs(f y - b) | y <- [x+1..x+100]] < a])

