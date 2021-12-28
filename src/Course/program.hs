
main :: IO ()  --typ programu '::' - has type
-- main=putStrLn "Hello World"

main= do 
    content <- readFile "course/numbers.txt"  -- '<-' - from
    putStrLn content