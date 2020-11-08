--  _                                               _               _        _ _
-- | | ___  __ _ _ __ _ __  _   _  ___  _   _  __ _| |__   __ _ ___| | _____| | |  ___ ___  _ __ ___
-- | |/ _ \/ _` | '__| '_ \| | | |/ _ \| | | |/ _` | '_ \ / _` / __| |/ / _ \ | | / __/ _ \| '_ ` _ \
-- | |  __/ (_| | |  | | | | |_| | (_) | |_| | (_| | | | | (_| \__ \   <  __/ | || (_| (_) | | | | | |
-- |_|\___|\__,_|_|  |_| |_|\__, |\___/ \__,_|\__,_|_| |_|\__,_|___/_|\_\___|_|_(_)___\___/|_| |_| |_|
--                          |___/

import Data.List


-- Hey, I (recursion)

fact :: Integral a => a -> a
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

-- Pattern matching

end :: [a] -> a
end [] = error "End of an empty array?? r u stupid?!"
end [x] = x
end (_:xs) = end xs

len :: Num b => [a] -> b
len [] = 0
len (_:xs) = 1 + len xs

-- Guards, where?

calcBmi :: RealFloat a => a -> a -> a
calcBmi mass height = mass / height^2

calcBmis :: RealFloat a => [(a, a)] -> [a]
calcBmis xs = [calcBmi m h | (m, h) <- xs]

bmiShow :: RealFloat a => a -> String
bmiShow bmi
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"
  | otherwise     = "You're a whale, congratulations!"
  where (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- Case expressions

add :: Num a => [a] -> a
add [] = 0
add (x:xs) = x + add xs

add' :: Num a => [a] -> a
add' xs = case xs of
            [] -> 0
            (x:xs) -> x + add' xs

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
                                      []        -> "an empty list."
                                      [_]       -> "a singleton list."
                                      otherwise -> "of many elements."

-- More recursion

max' :: Ord a => a -> a -> a
max' x y = if x > y then x else y

maximum' :: Ord a => [a] -> a
maximum' [] = error "Maximum of an empty sequence, are you insane?!"
maximum' [a] = a
maximum' (x:xs) = max' x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x : replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0     = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

repeatSlow' :: a -> [a]
repeatSlow' x = repeat' x ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs

-- Quick, sort!

insertToSorted :: Ord a => a -> [a] -> [a]
insertToSorted x [] = [x]
insertToSorted x (y:ys)
  | x <= y = x:y:ys
  | otherwise = y : insertToSorted x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insertToSorted x (insertionSort xs)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerSorted = quickSort [ x' | x' <- xs, x' <= x ]
      biggerSorted = quickSort [ x' | x' <- xs, x' > x ]
   in smallerSorted ++ [x] ++ biggerSorted

-- HOC

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
-- map' f xs = [ f x | x <- xs ]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x       = x : filter' f xs
  | otherwise = filter' f xs
-- filter' f xs = [ x | x <- xs, f x ]

quickSortFilt :: Ord a => [a] -> [a]
quickSortFilt [] = []
quickSortFilt (x:xs) = quickSortFilt (filter (<=x) xs) ++ [x] ++ quickSortFilt (filter (>x) xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = takeWhile' p xs


collatzChain :: Integral a => a -> [a]
collatzChain 1 = [1]
collatzChain n
  | even n = n : collatzChain (n `div` 2)
  | odd n  = n : collatzChain (n * 3 + 1)

-- Foldables

sum' :: Num a => [a] -> a
-- sum' [] = 0
-- sum' (x:xs) = x + sum' xs
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0

elem'' :: Eq a => a -> [a] -> Bool
elem'' e = foldl (\acc x -> acc || x == e) False

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []

sum'' :: Num a => [a] -> a
sum'' = foldl1 (+)

-- Dollars n Compositions

dollars = take 10 $ map (^2) $ map (/2) [1..]

-- map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
recomposed = map (negate . sum . tail) [[1..5],[3..6],[1..7]]

-- Data.List

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
     in foldl
         (\acc x -> acc || take nlen x == needle)
         False
         (tails haystack)
