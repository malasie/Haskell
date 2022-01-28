{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

--- Zad 20
---   Proszę obliczyć typ następujących funkcji



---  Zad 21
---   Niech będzie dana następująca definicja.
-- sum f l1 l2 = (f l1) + (f l2)
--- Dlaczego wyrażenie sum length [1,2] ['a','b'] jest błędne? 
-- Proszę poprawić definicję funkcji sum tak, aby wyrażenie sum length [1,2] ['a','b'] było poprawnie.



sum f1 f2 l1 l2 = (f1 l1) + (f2 l2) 

