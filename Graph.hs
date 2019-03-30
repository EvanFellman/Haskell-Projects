module Graph(getVertexes, getEdges, insertEdge, insertDEdge, emptyGraph, isEdge) where
	data Graph a = Graph [a] [(a, a)] deriving Show

	getVertexes:: Graph a -> [a]
	getVertexes (Graph a _) = a

	getEdges:: Graph a -> [(a, a)]
	getEdges (Graph _ b) = b

	emptyGraph = Graph [] []

	insertEdge (Graph a b) ye@(x, y) = Graph (inser' a x y False False [])  (insertIfHaveTo b ye) where
		insertIfHaveTo [] u	= [u]
		insertIfHaveTo yeet@(x: xs) u
			| x == u 			= yeet
			| otherwise			= x: (insertIfHaveTo xs u)
		inser' [] u v uu vv acc
			| not uu 			= if not vv 
				then u: (v: acc)
				else u: acc
			| otherwise			= if not vv 
				then v: acc 
				else acc
		inser' g@(x:xs) u v uu vv acc
			| uu || (x == u) 	= if x == v || vv
				then g++acc
				else inser' xs u v True False (x: acc)
			| vv || (x == v) 	= inser' xs u v False True (x: acc)
			| otherwise 		= inser' xs u v False vv (x: acc)
	
	insertDEdge (Graph a b) (x, y) = Graph (inser' a x y False False []) (insertIfHaveTo (insertIfHaveTo b (y, x)) (x, y)) where
		insertIfHaveTo [] u	= [u]
		insertIfHaveTo yeet@(x: xs) u
			| x == u 			= yeet
			| otherwise			= x: (insertIfHaveTo xs u)
		inser' [] u v uu vv acc
			| not uu 			= if not vv 
				then u: (v: acc)
				else u: acc
			| otherwise			= if not vv 
				then v: acc 
				else acc
		inser' g@(x:xs) u v uu vv acc
			| uu || (x == u) 	= if x == v || vv
				then g++acc
				else inser' xs u v True False (x: acc)
			| vv || (x == v) 	= inser' xs u v False True (x: acc)
			| otherwise 		= inser' xs u v False vv (x: acc)
	
	isEdge:: Eq a => Graph a -> (a,a) -> Bool
	isEdge (Graph _ xs) a 	= helper xs a where
		helper [] _ 		= False
		helper (x:xs) a 
			| x == a 		= True
			| otherwise 	= helper xs a