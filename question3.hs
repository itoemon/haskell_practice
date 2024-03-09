mySort :: (Ord a) => [a] -> [a]
mySort [] = []
mySort (x:xs) = mySort [ a | a <- xs, a < x ]
                      ++ [x] ++
                mySort [ a | a <- xs, a >= x ]
              
uniqList :: (Eq a) => [a] -> [a]
uniqList [] = []
uniqList (x:xs) = (if x `elem` xs then [] else [x]) ++ (uniqList xs)

myTake :: Float -> [a] -> [a]
myTake 0 _ = []
myTake n [] = []
myTake n (x:xs) = x : myTake (n-1) xs


takeXiyjzk :: (Num a, Ord a, Eq a) => 
  Float -> a -> a -> a -> [a]
takeXiyjzk n x y z =
  myTake n (
    mySort (
      uniqList ([ (x^i) * (y^j) * (z^k) | 
      i <- [0..( ceiling ( logBase 3 n ) )], 
      j <- [0..( ceiling ( logBase 3 n ) )], 
      k <- [0..( ceiling ( logBase 3 n ) )]
      ])
    )
  )

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x:xs) | p x =  x : myTakeWhile p xs
                     | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p [] = []
myDropWhile p (x:xs) | p x =  myDropWhile p xs
                     | otherwise = x : xs

betweenXiyjzk :: 
  Int -> Float -> Float -> Int -> Int -> [Int]
betweenXiyjzk n m x y z =
  myTakeWhile ( < (( ceiling m ) + 1 ) ) (
    myDropWhile ( < n )  (
      mySort (
        uniqList ([ ( ( ceiling x ) ^i) * (y^j) * (z^k) | 
        i <- [0..( ceiling ( logBase x m ) - 1)], 
        j <- [0..( ceiling ( logBase x m ) - 1)], 
        k <- [0..( ceiling ( logBase x m ) - 1)]
        ])
      )
    )
  )
