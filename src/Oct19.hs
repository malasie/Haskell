{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Oct19
    (delete
    ) where

append :: [a] -> [a] -> [a]
append [] m = m
append (x:xs) m = x: append xs m


member x [] = False
member x (y:ys) = if x==y then True else member x ys

last (x:xs) = if length xs==0 then x else Oct19.last xs


delete x (l:ls) = if x /= l then l:delete x ls else ls



pairing (x:xs) (y:ys) = if xs==[]&& ys==[] then [(x,y)] else append [(x,y)] (pairing xs ys)


split x l = [[l1 | l1<-l, l1<x], [l2 | l2<-l, not(l2<x)]]


map f (x:xs) = if xs==[] then [f x] else (f x) : (Oct19.map f xs)



map2 f (x:xs) (y:ys)= if xs==[] && ys==[] then [f x y] else (f x y) : (map2 f xs ys)


filter p l = [a| a<- l, p a]


