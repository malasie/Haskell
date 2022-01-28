import Control.Arrow (Arrow(first))
eliminate n []=[]
eliminate n (y:ys)= if n==0 then (y:ys) else eliminate (n-1) ys

g[] = Nothing
g[x]= Just x
g(x:y:l)=if x<y then Just x else Just y


count x l = length( filter (==x) l)




transform []=[]
transform [x]=[(x,1)]
transform (x:xs)=  (x, i):(transform (tail2 i (x:xs))) where
    i = (length [y| y<-xs, x==y]+1)
    tail2 a (y:ys)= if a==1 then ys else tail2 (a-1) ys


data Tree a b = Leaf a | Node b (Tree a b) (Tree a b)

sumT (Leaf a) = a
sumT (Node b l r)= sumT l + sumT r

maxT (Leaf a) = a
maxT (Node b l r) = maximum [b, maxT l, maxT r]

mapT f (Leaf a) = Leaf a
mapT f (Node b l r)= Node (f b) (mapT f l) (mapT f r)

iter 0 f x =x
iter n f x= (^) (f x) n



dz test koniec dziel polacz p =
  if test p
  then koniec p
  else polacz (map (dz test koniec dziel polacz) (dziel p))
 
 

mergesort p = dz (\x -> length x <= 1)
               (\x -> x)
               half
               merge p
 
half l = half_help l [] (div (length l) 2)
   where half_help l m 0      = [m,l]
         half_help (x:xs) m n = half_help xs (m ++ [x]) (n-1)
 
merge :: Ord a => [[a]] -> [a]
merge [xs, []] = xs
merge [[], ys] = ys
merge [(x:xs), (y:ys)] | x <= y    = x:merge [xs, (y:ys)]
                    | otherwise = y:merge [(x:xs), ys]
 



integer_length u = if u < 10
                   then 1
                   else 1 + integer_length (div u 10)
 
 
test x = n == 0 where (u,v,n) = x
 
koniec x = (u * v, n) where (u,v,n) = x
 
dziel (u,v,n) = 
   let m  = div n 2
       u0 = mod u (10 ^ m)
       v0 = mod v (10 ^ m)
       u1 = div u (10 ^ m)
       v1 = div v (10 ^ m)
   in [(u1,v1,m), (u0,v0, m), (u1-u0,v0-v1,m)]
 
comb [(p1,m1),(p2,m2),(p3,m3)] =
   if m1 == m2 && m1 == m3
   then ( (10 ^ (2*m1) + 10 ^ m1) * p1 + 10 ^ m1 * p3 + (10 ^ m1 + 1) * p2, 2 * m1)
   else (0,0)