
module Tree.Rose where 

data Tree b a = Node a [Tree b a] | Leaf b
    deriving Show

instance Functor (Tree b) where
    fmap _ (Leaf x) = Leaf x
    fmap f (Node x ns) = Node (f x) (map (fmap f) ns)
