myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n [] = []
myTake n (x:xs) = x : myTake (n-1) xs

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x:xs) | p x =  x : myTakeWhile p xs
                     | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p [] = []
myDropWhile p (x:xs) | p x =  myDropWhile p xs
                     | otherwise = x : xs