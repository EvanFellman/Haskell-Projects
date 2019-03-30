module Main (main) where
import Queue
import BinTree
import LinkedList
fib 0 = 1
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

fib' n = helper n 1 1 1
    where helper n a b i 
            | i == n    = a
            | otherwise = helper n (a + b) a (i + 1) 

foo 0 = 0
foo 1 = 1
foo n = (foo (n - 1)) - (foo (n - 2))

main = do
    print "Hello world!"