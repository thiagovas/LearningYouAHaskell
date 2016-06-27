-- BST - Binary Search Tree.
data BTree a = Leaf | Node a (BTree a) (BTree a)


-- Method to insert an element to the BST.
insert elem Leaf = Node elem Leaf Leaf
insert elem t@(Node a left right)
     | elem == a  =  t
     | elem < a   =  Node a (insert elem left) right
     | otherwise  =  Node a left (insert elem right)


-- Function that searches an element through the BST.
search _ Leaf = False
search elem t@(Node a left right) = if elem == a
                                        then True
                                        else if elem < a
                                        then search elem left
                                        else search elem right


-- BEGIN TESTING
foldSearch _ z Leaf = z
foldSearch f z (Node a left right) = r || r1 || r2
            where
              r = f a z
              r1 = foldSearch f z left
              r2 = foldSearch f z right

searchFold elem = foldSearch (== elem) False
-- END TESTING


-- Fold that executes in pos order on the BST.
foldposorder _ z Leaf = z
foldposorder f z (Node a left right) = f z2 a
              where
                z1 = foldposorder f z left
                z2 = foldposorder f z1 right


-- Function that creates a BST from a list.
makeTree [] = Leaf
makeTree (a:x) = insert a (makeTree x)


-- Function that creates a BST from a list.
--   Using fold
makeTreeFold x = foldr insert Leaf x


-- Function that returns the largest element of a BST.
getLargest Leaf = Leaf
getLargest (Node a _ Leaf) = a
getLargest (Node a _ right) = getLargest right


-- Function that removes the largest element of a BST.
removeLargest Leaf = Leaf
removeLargest (Node a left Leaf) = left
removeLargest (Node a left right) = (Node a left (removeLargest right))


-- Function that removes any element of a BST.
remove _ Leaf = Leaf
remove elem (Node a left right)
     | elem == a  = (Node (getLargest left) (removeLargest left) right)
     | elem < a   = (Node a (remove elem left) right)
     | otherwise  = (Node a left (remove elem right))
