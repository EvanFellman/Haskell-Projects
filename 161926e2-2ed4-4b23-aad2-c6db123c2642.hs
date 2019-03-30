module Main (main) where
import Queue

data Tree a = TN a (Tree a) (Tree a) | Leaf a | EmptyTree

instance Show a => Show (Tree a) where
    show EmptyTree = ""
    show (Leaf a) = show a
    show (TN a EmptyTree EmptyTree) = show a
    show (TN a left EmptyTree) = (show a) ++ ": left: " ++ (show left)
    show (TN a EmptyTree right) = (show a) ++ ": right: " ++ (show right)
    show (TN a left right) = (show a) ++ ": left: " ++ (show left) ++ ", right: " ++ (show right)

fib 0 = 1
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

fuck _ = print "no u"

fib' n = helper n 1 1 1
    where helper n a b i 
            | i == n    = a
            | otherwise = helper n (a + b) a (i + 1) 





main = do
    a <- getLine
    print (fib' (read a))
    print (TN 5 (TN 2 EmptyTree (Leaf 3)) EmptyTree)