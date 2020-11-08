
main :: IO ()
main = putStr . foldl1 (++) . map (++"\n") . map fizzer $ [1..100]


fizzer :: Int -> String
fizzer n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3  == 0 = "Fizz"
  | n `mod` 5  == 0 = "Buzz"
  | otherwise       = show n
