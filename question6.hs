downTo0 :: (Num a, Eq a) => a -> [a]
downTo0 0 = [0]
downTo0 x = x : downTo0 (x - 1)

zipPlus :: (Num a, Eq a) => ([a],[a]) -> [a]
zipPlus ((x : xs),(y : ys)) =
  x + y : zipPlus (xs,ys)
zipPlus ((x : xs),[]) = []
zipPlus ([],(x : xs)) = []

anaList :: (Num b, Eq b) => 
  (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
anaList p f g x | (p x) = []
                | otherwise = 
                  (f x) : (anaList p f g (g x))

downTo0_v2 :: (Num a, Ord a) => a -> [a]
downTo0_v2 x =
  anaList (\x -> x < 0)
          id
          (\x -> x - 1)
          x

zipPlus_v2 :: (Num a, Eq a) => ([a],[a]) -> [a]
zipPlus_v2 ((x : xs),(y : ys)) =
  anaList ( \((x : xs),(y : ys)) -> 
              null (x : xs) || null (y : ys) )
          ( \((x : xs),(y : ys)) -> x + y )
          ( \((x : xs),(y : ys)) -> ( xs , ys ) )
          ((x : xs),(y : ys))