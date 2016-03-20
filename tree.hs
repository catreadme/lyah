data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x Empty Empty

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = singleton x
insert x (Node s left right)
  | x == s = Node x left right
  | x > s = Node s (insert x left) right
  | x < s = Node s left (insert x right)

check :: (Ord a) => a -> Tree a -> Bool
check _ Empty = False
check x (Node s left right)
  | x == s = True
  | x < s = check x left
  | x > s = check x right
