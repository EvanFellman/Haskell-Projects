module Node(Node(N, Null), info, next) where
data Node a = N a (Node a) | Null
instance Show a => Show (Node a) where
	show Null = "null"
	show (N a b) = (show a) ++ " -> " ++ (show b)

info:: Node a -> a
info (N a _) = a

next:: Node a -> Node a
next (N _ a) = a