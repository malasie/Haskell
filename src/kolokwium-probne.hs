{-# LANGUAGE FlexibleContexts #-}
import System.Win32 (xBUTTON1)
data Tree a = Nil | Node a (Tree a) (Tree a)

--ilość liści
tree_size Nil = 1
tree_size (Node a l r) = (tree_size l) + (tree_size r)

-- liście + węzły
tree_size2 Nil = 1
tree_size2 (Node a l r) = 1+ (tree_size2 l) + (tree_size2 r)

tree_max :: (Ord a) => Tree a -> Maybe a 
tree_max Nil = Nothing
tree_max (Node a l r) = maximum [Just a, tree_max l, tree_max r] 

palindrom :: Eq a => [a] -> Bool
palindrom l = if l==(reverse l) then True else False

member x [] = False
member x [y] = if x==y then True else False
member x (y:ys) = if x==y then True else member x ys

exists pred []= False 
exists pred [x]= pred x
exists pred (x:xs) = if pred x==False then exists pred xs else True

exists2 pred = foldl (\x y -> (pred y) || x) False

cut [] =[]
cut (y:ys)= if (length (y:ys))<2 then [] else init ys

cut2 [] =[]
cut2 l= if length l <2 then [] else tail(init l)


maptree f Nil =Nil
maptree f (Node a Nil Nil)= Node (f a) Nil Nil
maptree f (Node a l r)= Node (f a) (maptree f l) (maptree f r)


add_length Nil = Nil
add_length (Node a l r)= Node ((length a):a) (add_length l) (add_length r)

