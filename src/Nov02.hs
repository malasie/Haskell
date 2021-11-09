{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Nov02
    (
    ) where
import Distribution.Simple.Utils (xargs)

--- Zad 17
---  Jaki typ mają następują wyrażenia?

-- a) (+) :: Num a => a -> a -> a
-- b) (+) 37 :: Num a => a -> a
-- c) append :: [a] -> [a] -> [a]
-- d) append [1,2] :: Num a => [a] -> [a]
-- e) map :: (a -> b) -> [a] -> [b]
-- f) map square [1,2,3,4,5] :: Num b => [b]
-- g) map square [['a']]  ????
-- h) map length [['a']] :: [Int]
-- i) foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- j) foldl (++) :: Foldable t => [a] -> t [a] -> [a]
-- k) foldl (++) [] :: Foldable t => t [a] -> [a]
-- l) f 7   ?????
-- m) \f -> f 7 :: Num t1 => (t1 -> t2) -> t2
-- n)   + (f x) (g x)    ????
-- o)   f 7 (g 'x')   ?????
-- p)   \f -> f (g x)   ???
-- q)   (\f -> f (g x)) square  ??






--- Zad 16
---  Proszę napisać funkcję dz, która realizuje schemat do rozwiązania problemów typu "dziel i zwyciężaj" (divide and conquer) na abstrakcyjnym poziomie. dz ma używać następujące argumenty:
----    1. test, który sprawdza czy przypadek jest triwialny
----    2. koniec, który rozwiązuje triwialny przypadek
----    3. dziel, który dzieli problem do podproblemów
----    4. połącz, który połączy rozwiązania podproblemów
---  Używając funkcję dz proszę zdefiniować funkcję mergesort oraz mnożenie według Karatsuby.


mergesort :: Ord a => [a] -> [a]
mergesort p = dz (\x -> length x <= 1)
               (\x -> x)
               half
               merge p


dz :: (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) -> a -> b
dz test koniec dziel polacz p = if test p then koniec p else polacz(map(dz test koniec dziel polacz)(dziel p))

half l = [drop (length l `div` 2) l, take (length l `div` 2) l]



merge [x:xs,y:ys] = if xs==[] && ys==[] then (if x < y
                        then x : y:[]
                        else y : x:[] )
                    else (if ys==[] then (if x < y
                            then  x : y:xs
                            else y : x:xs)
                        else (if x < y
                    then x : (merge [xs,y:ys])
                    else y : (merge [x:xs,ys])))

                    --funkcja ma lekkie błędy