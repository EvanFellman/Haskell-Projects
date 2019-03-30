module Queue(newQueue, enqueue, dequeue, isEmpty) where
    data Queue a = Queue [a] [a]

    newQueue:: Queue a
    newQueue = Queue [] []

    enqueue:: Queue a -> a -> Queue a
    enqueue (Queue fronts backs) a = Queue fronts (a: backs)

    dequeue:: Queue a -> (a, Queue a)
    dequeue (Queue [] backs)        = dequeue (Queue (rev backs []) [])
        where 
            rev:: [a] -> [a] -> [a]
            rev [] acc              = acc
            rev (x:xs) acc         = (rev xs (x:acc))
    dequeue (Queue (x:xs) backs)   = (x, Queue xs backs)

    isEmpty:: Queue a -> Bool
    isEmpty (Queue [] [])   = True
    isEmpty _               = False

    instance (Show a) => Show(Queue a) where
        show (Queue [] [])          = ""
        show q
            | isEmpty b             = show a
            | otherwise             = (show a) ++ ", " ++ (show b) where
                (a, b)              = dequeue q   