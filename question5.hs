data BETree a = Leaf a 
                | SNode a (BETree a) 
                | DNode a (BETree a) (BETree a) 
                deriving (Eq, Show)

tree :: BETree Int
tree = DNode 5 (DNode 8 (Leaf 3) (SNode 1 (Leaf 7)))
               (DNode 6 (SNode 2 (Leaf 9)) (Leaf 4))


depthBETree :: (Num a, Ord a) => BETree a -> a
depthBETree (Leaf x) = 0
depthBETree (SNode x t) = 1 + depthBETree t
depthBETree (DNode x lt rt) 
            | depthBETree lt > depthBETree rt 
              = depthBETree (SNode x lt)
            | otherwise 
              = depthBETree (SNode x rt)

sumBETree :: (Num a) => BETree a -> a
sumBETree (Leaf x) = x
sumBETree (SNode x t) = x + sumBETree t 
sumBETree (DNode x lt rt) = x + sumBETree lt + sumBETree rt

upAccBETree :: (Num a) => BETree a -> BETree a
upAccBETree (Leaf x) = Leaf x
upAccBETree (SNode x t) = SNode (sumBETree (SNode x t)) (upAccBETree t)
upAccBETree (DNode x lt rt) = DNode (sumBETree (DNode x lt rt)) 
                                    (upAccBETree lt)
                                    (upAccBETree rt)


catBETree :: 
  (BETree a -> b) -> (BETree a -> a) 
  -> (a -> b -> b) -> (a -> b -> b -> b) 
  -> BETree a -> b
catBETree fLeaf fNode g g2 (Leaf x) = fLeaf (Leaf x)
catBETree fLeaf fNode g g2 (SNode x t) = 
  g (fNode (SNode x t)) (catBETree fLeaf fNode g g2 t)
catBETree fLeaf fNode g g2 (DNode x lt rt) = 
  g2 (fNode (DNode x lt rt))
     (catBETree fLeaf fNode g g2 lt) 
     (catBETree fLeaf fNode g g2 rt)


depthBETree2 :: (Num a, Ord a) => BETree a -> a
depthBETree2 t =
  catBETree (\t -> 0) 
            (\t -> 1) 
            (\x t -> x + t) 
            (\x l r -> if l > r then x + l else x + r) 
            t

getValue :: BETree a -> a
getValue (Leaf x) = x
getValue (SNode x t) = x
getValue (DNode x l r) = x

sumBETree2 :: (Num a) => BETree a -> a
sumBETree2 t = 
  catBETree (\t -> (getValue t))
            (\t -> (getValue t))
            (\x t -> x + t) 
            (\x l r -> x + l + r) 
            t

upAccBETree2 :: (Num a) => BETree a -> BETree a
upAccBETree2 t = 
  catBETree id
            (\t -> sumBETree2 t)
            (\x t -> SNode x t)
            (\x l r -> DNode x l r)
            t















