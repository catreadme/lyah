{- TODO add Maybe to failable functions -}
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show)

data Direction = L | R
  deriving (Show)

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a)
  deriving (Show)

type Directions = [Direction]
type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a)

tree :: Tree Char
tree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

myElem = elemAt [L,R,L] tree -- 'S'

goLeft :: (Tree a,Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r,bs) = (l,LeftCrumb x r:bs)

goRight :: (Tree a,Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r,bs) = (r,RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

x -: f = f x

myTree = (tree,[]) -: goLeft -: goRight

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

myMod = modify (\_ -> 'J') $ (tree,[]) -: goLeft

attach :: Tree a -> Zipper a -> Zipper a
attach t (_,bs) = (t, bs)

myAtt = attach (Node 'R' Empty Empty) $ (tree,[]) -: goLeft -: goLeft -: goLeft -: goLeft

top :: Zipper a -> Zipper a
top (t,[]) = (t,[])
top z = top $ goUp z

myTop = top $ (tree,[]) -: goLeft
