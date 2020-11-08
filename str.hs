
main :: IO ()
main = putStrLn (boxed (joinWith '\n' (splitOn ' ' "hello how are you doing kiddo")))

fixedWidth :: Int -> Char -> String -> String
fixedWidth 0 _ _ = ""
fixedWidth n c "" = replicate n c
fixedWidth n c (x:xs) = x:fixedWidth (n-1) c xs

boxed :: String -> String
boxed raw = joinWith '\n' [lid,body,lid]
    where content = splitOn '\n' raw
          width = maximum [ length line | line <- content ]
          fenced x = "| " ++ fixedWidth width ' ' x ++ " |"
          body = joinWith '\n' [ fenced line | line <- content ]
          lid = "+" ++ fixedWidth (width+2) '-' "" ++ "+"

joinWith :: Char -> [String] -> String
joinWith _ [] = ""
joinWith _ [str] = str
joinWith c (str:strs) = str ++ [c] ++ joinWith c strs

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c [char]
  | char == c = ["",""]
  | otherwise = [[char]]
splitOn c (char:chars)
  | char == c = "":rest
  | otherwise = (char:head rest):tail rest
  where rest = splitOn c chars
