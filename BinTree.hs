module BinTree(Tree(TN, EmptyTree), info, left, right) where
import Queue

data Tree a = TN a (Tree a) (Tree a) | EmptyTree deriving Show

instance Show a => Tree a where
	show a = helper (enqueue newQueue a) where
		helper:: Tree a -> Queue a -> String
		helper e
			| isEmpty e = ""
			| otherwise = where
				(x, y) = dequeue e

info:: Tree a -> a
info 	(TN a _ _) = a

left:: Tree a -> Tree a
left 	(TN _ a _) = a

right:: Tree a -> Tree a
right	(TN _ _ a) = a