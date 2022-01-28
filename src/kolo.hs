

take p [x]= if p x then [x] else []
take p (x:xs) = if not(p x)  then [] else x: (Main.take p xs)

a x= (>2) x
f a b c = if a b then a b else a c