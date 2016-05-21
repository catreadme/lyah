{- TODO add Maybe to failable functions -}
type Zipper a = ([a],[a])

forward :: Zipper a -> Zipper a
forward (x:xs,bs) = (xs, x:bs)

back :: Zipper a -> Zipper a
back (xs,b:bs) = (b:xs, bs)

myFor = forward ([1,2,3],[]) -- [2,3],[1])
myBa = back $ forward $ forward ([1,2,3],[]) -- [2,3],[1])
