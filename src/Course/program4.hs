import System.Win32 (RegInfoKey(values))
import Data.ByteString (count)
readInts::String -> [Int] -- '->' - match to
readInts s= let ws =words s in map read ws

minMax :: Ord a => [a] -> Maybe (a,a) -- "=>" - implies 
minMax (h : t) = Just $ foldr -- ':' - comes; from h to t
    (\x (min, max) -> (if x< min then x else min, if x > max then x else max))
        (h,h)
        t
minMax _ = Nothing 

-- 'Just' and 'Nothing' are data constractors that belong to 'Maybe' type 

main:: IO()
main = do
    content <- readFile "course/numbers.txt"  -- '<-' - from
    let values = readInts content
        count = length values
        total = sum values 
        mean = fromIntegral total / fromIntegral count
        range = minMax values
    print count
    print total
    print mean
    print range