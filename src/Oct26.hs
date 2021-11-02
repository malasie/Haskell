{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Oct26
    (Oct26.length
    ) where
import Distribution.Simple.Utils (xargs)

--- Zad 12
--- Proszę zdefiniować funkcję foldr analogicznie do funkcji foldl. Jaki ma typ funkcja foldr?

----    różnica między foldl a foldr jest sposób składania
----    np.  
-----         foldl (-) 0 [1,2,3,4]       ((((0 - 1) - 2) - 3) - 4) = -10
-----         foldr (-) 0 [1,2,3,4]       (1 - (2 - (3 - (4 - 0)))) = -2

foldl2 f e [] =e
foldl2 f e (x:xs) = foldl2 f (f e x) xs

foldr3 f v [] = v
foldr3 f v (x:xs) = f x (foldr f v xs)


 --- Zad 13
 --- Używając funkcje foldl i foldr proszę zdefiniować funkcje
prod ys = foldr (\ y -> (*) y) 1 ys

length ys = foldr (\ y -> (+) 1) 0 ys

reverse l = foldl (flip (:)) [] l

and ls= foldr (&&) True  ls

nwd (y:ys) = foldr gcd y ys

map f = foldr ((:).f) []

delete x = foldr (\l ls -> if x/=l then l:ls else ls)[]

filter pred = foldr (\x xs -> if pred x then x:xs else xs) []

forall pred = foldl (\x y -> (pred y) && x) True



---Zad 14
---	Proszę zdefiniować funkcję insertionsort :: (a -> a -> Bool) -> [a] -> [a] przy pomocy kombinatora.




---Zad 15
---	Proszę napisać funkcję iter n f dla jednoargomentowej funkcji f i liczby naturalnej n. Wartością funkcji jest funckją, która obliczy fn.
iter :: (Integral b, Num a) => b -> (t -> a) -> t -> a
iter n f x= (^) (f x) n 