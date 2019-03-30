module LinkedList(emptyList, isEmpty, size, insertFirst, insertLast, insertAt, removeFirst, removeLast, removeAt, getFirst, getLast, get, indexOf) where
import Node

emptyList:: Node a
emptyList = Null

isEmpty:: Node a -> Bool
isEmpty Null 	= True
isEmpty _		= False

size:: Node a -> Int
size Null = 0
size n = 1 + (size (next n))

insertFirst:: Node a -> a -> Node a
insertFirst node a = N a node

insertLast:: Node a -> a -> Node a
insertLast Null a = N a Null
insertLast node a = N (info node) (insertLast (next node) a)

insertAt:: Node a -> Int -> a -> Node a
insertAt node 0 elem 		= N elem node
insertAt node index elem 	= N (info node) (insertAt (next node) (index - 1) elem)

removeFirst:: Node a -> Node a
removeFirst (N _ b) = b

removeLast:: Node a -> Node a
removeLast (N a Null) = Null
removeLast (N a b) = N a (removeLast b)

removeAt:: Node a -> Int -> Node a
removeAt (N a b) 0 = b
removeAt (N a b) i = N a (removeAt b (i - 1))

getFirst:: Node a -> a
getFirst (N a _) = a

getLast:: Node a -> a
getLast (N a Null) = a
getLast (N _ b) = getLast b

get:: Node a -> Int -> a
get (N a _) 0 = a
get (N _ b) i = get b (i - 1)

indexOf:: Node a -> a -> Int
indexOf n elem = helper n elem 0 where
	helper (N a b) elem i
		| a == elem = i
		| otherwise = helper b elem (i + 1)