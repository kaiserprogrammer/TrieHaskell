module Trie(Trie(..),createRoot,add,del,change,points) where

data Trie = Nil | T [Trie] Char (Maybe Int) deriving (Show,Eq,Ord)

createRoot :: Trie
createRoot = T [] '+' Nothing

add :: Trie -> String -> Int -> Trie
add Nil _ _ = error "Trie is not existent"
add (T _ '+' _) [] _ = error "Cannot change root"
add (T tries chr pnts) [] newP = T tries chr (Just newP)
add (T tries chr pnts) (x:xs) newP =
  case exists tries x of
  (False,_) -> T ((add (T [] x Nothing) xs newP):tries) chr pnts
  (True,index) -> T (take index tries ++ (add (tries!!index) xs newP : drop (index + 1) tries)) chr pnts

exists :: [Trie] -> Char -> (Bool,Int)
exists [] _ = (False,0)
exists ((T tries tChr pnts):xs) chr
  | tChr == chr = (True,0)
  | otherwise = (fst (exists xs chr), (snd (exists xs chr) + 1)) 

delete :: Trie -> String -> Trie
delete (T _ '+' _) [] = error "Cannot delete root"
delete (T tries chr pnts) [] = T tries chr Nothing
delete (T tries chr pnts) (x:xs) =
  case exists tries x of
  (False,_) -> T tries chr pnts
  (True,index) -> T (take index tries ++ (delete (tries!!index) xs : drop (index + 1) tries)) chr pnts

cleanup :: Trie -> String -> [String]
cleanup (T _ '+' _) [] = []
cleanup (T tries chr pnts) [] 
  | tries == [] = []
  | otherwise = ["                         "]
cleanup (T tries chr pnts) (x:xs)
  | pnts == Nothing =
    case exists tries x of
    (False,_) -> []
    (True,index) -> cleanup (tries!!index) xs
  | otherwise = 
    case exists tries x of
    (False,_) -> [(x:xs)]
    (True,index) -> [(x:xs)] ++ cleanup (tries!!index) xs

delTrie :: Trie -> String -> String -> Trie
delTrie root@(T _ '+' _) [] _ = root
delTrie trie _ [] = trie
delTrie trie [] _ = trie
delTrie (T tries chr pnts) str1@(x:xs) str2
  | str1 == str2 = case exists tries x of
    (False,_) -> T tries chr pnts
    (True,index) -> T (take index tries ++ drop (index + 1) tries) chr pnts
  | otherwise = case exists tries x of
    (False,_) -> T tries chr pnts
    (True,index) -> T (take index tries ++ (delTrie (tries!!index) xs str2) : drop (index + 1) tries) chr pnts

del :: Trie -> String -> Trie
del trie str =  delTrie (delete trie str) str (last (cleanup (delete trie str) str))

change :: Trie -> String -> Int -> Trie
change root@(T _ '+' _) [] _ = root
change (T tries chr pnts) [] newP = T tries chr (Just newP)
change (T tries chr pnts) (x:xs) newP =
  case exists tries x of
  (False,_) -> T tries chr pnts
  (True,index) -> T (take index tries ++ (change (tries!!index) xs newP) : drop (index + 1) tries) chr pnts

points :: Trie -> String -> Maybe Int
points (T tries chr pnts) [] = pnts
points (T tries chr pnts) (x:xs) =
  case exists tries x of
  (False,_) -> Nothing
  (True,index) -> points (tries!!index) xs
