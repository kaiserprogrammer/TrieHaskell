module Shell where

import Trie

data Befehl = Add | Del | Change | Points | Root | Help |Quit | Show

main :: IO ()
main = programmLoop createRoot

programmLoop :: Trie -> IO ()
programmLoop trie = do
  putStr "trie> "
  befehl <- getLine
  let inBefehl = interpBefehl befehl
  case inBefehl of
   (Add,(Just key),(Just pnts)) -> programmLoop (add trie key pnts)
   (Del,Just key,_) -> programmLoop (del trie key)
   (Change,Just key,Just pnts) -> programmLoop (change trie key pnts)
   (Points,Just key,_) -> 
      putStrLn ("key points: " ++ show (points trie key)) >>
      programmLoop trie
   (Root,_,_) -> programmLoop (createRoot)
   (Help,_,_) -> do
       putStrLn "Help!!!"
       programmLoop trie
   (Quit,_,_) -> return ()
   (Show,_,_) -> putStrLn (show trie) >>
                 programmLoop trie 
   (_,_,_) -> programmLoop trie


interpBefehl :: String -> (Befehl,Maybe String,Maybe Int)
interpBefehl [] = (Help, Nothing,Nothing)
interpBefehl xs@(x:xss) = case x of
  'a' -> (Add,Just (interpArgument xs),Just (read (interpInt xs)))
  'n' -> (Root,Nothing,Nothing)
  'd' -> (Del, Just (interpArgument xs),Nothing)
  'c' -> (Change, Just (interpArgument xs), Just (read (interpInt xs)))
  'p' -> (Points, Just (interpArgument xs), Nothing)
  'q' -> (Quit,Nothing,Nothing)
  't' -> (Show,Nothing,Nothing)
  _ -> (Help, Nothing, Nothing)

interpArgument :: String -> String
interpArgument = (getWord . dropSpace . dropWord . dropSpace)

interpInt :: String -> String
interpInt = (getInt . dropSpace . dropWord . dropSpace . dropWord . dropSpace)

dropWord :: String -> String
dropWord [] = []
dropWord str@(x:xs)
  | x `elem` whitespace = str
  | otherwise = dropWord xs

dropSpace :: String -> String
dropSpace [] = []
dropSpace str@(x:xs)
  | x `elem` whitespace = dropSpace xs
  | otherwise = str

whitespace :: [Char]
whitespace = [' ','\n','\t']

getWord :: String -> String
getWord [] = []
getWord (x:xs)
  | x `elem` whitespace = []
  | otherwise = x : getWord xs

getInt :: String -> String
getInt [] = []
getInt (x:xs)
  | isDigit x = x : getInt xs
  | otherwise = []

isDigit :: Char -> Bool
isDigit x = x `elem` ['0'..'9']

