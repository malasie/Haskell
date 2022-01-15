data Tree a b = Leaf a | Node a b (Tree a b) (Tree a b)

sumTree (Leaf a)= a
sumTree (Node a b l r)=a+ b+ (sumTree l)+ (sumTree r)

flatten [] = []
flatten [[a]] = [a]
flatten (x: xs) = x++ flatten xs



preTree :: Tree a a -> [a]
preTree (Leaf a)= [a]
preTree (Node e b l r)= e: flatten((preTree l) :[b : (preTree r)])

mapb f (Leaf a)= Leaf a
mapb f (Node a b (Leaf c) (Leaf d))= Node a (f b) (Leaf c) (Leaf d)
mapb f (Node a b l r) = Node a (f b) (mapb f l) (mapb f r)

x :: Integer
x=2

y::Integer 
y=3