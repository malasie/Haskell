module Main(main) where
data MyType=MyDataCOnstructor String 
class MyClass a where
    name:: a -> String

instance MyClass MyType where
    name (MyDataCOnstructor name) =name

func x = x
func' y=y

main = do
    print $ name (MyDataCOnstructor "Haskell")
    print $ 100 +200
    print $ (+) 100 200