

main :: IO ()
main = putStrLn $ show $ calcGcd 12 56


calcGcd :: Int -> Int -> Int
calcGcd x 0 = x
calcGcd x y = gcd (min x y) (max x y)


div3 :: Int -> Bool
div3 n
  | n == 0 = True
  | n < 3 = False
  | n == 3 = True
  | n == 6 = True
  | n == 9 = True
div3 n = div3 $ sum $ map (\c -> ((read :: String -> Int) (show c))) (show n)
