-- BST - Binary Search Tree
data BTree a = Leaf | Node a (BTree a) (BTree a)

insert elem Leaf = Node elem Leaf Leaf
insert elem t@(Node a left right)
     | elem == a  =  t
     | elem < a   =  Node a (insert elem left) right
     | otherwise  =  Node a left (insert elem right)


search _ Leaf = False
search elem t@(Node a left right) = if elem == a
                                        then True
                                        else if elem < a
                                        then search elem left
                                        else search elem right


getLargest Leaf = Leaf
getLargest (Node a _ Leaf) = a
getLargest (Node a _ right) = getLargest right

removeLargest Leaf = Leaf
removeLargest (Node a left Leaf) = left
removeLargest (Node a left right) = (Node a left (removeLargest right))

remove _ Leaf = Leaf
remove elem (Node a left right)
     | elem == a  = (Node (getLargest left) (removeLargest left) right)
     | elem < a   = (Node a (remove elem left) right)
     | otherwise  = (Node a left (remove elem right))
