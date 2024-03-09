mySort :: (Ord a) => [a] -> [a]
mySort [] = []
mySort (x:xs) = mySort [ a | a <- xs, a < x ]
                      ++ [x] ++
                mySort [ a | a <- xs, a >= x ]
              
uniqList :: (Eq a) => [a] -> [a]
uniqList [] = []
uniqList (x:xs) = (if x `elem` xs then [] else [x]) ++ (uniqList xs)

merge3 :: ( Ord a , Eq a ) => [a] -> [a] -> [a] -> [a]       
merge3 (x:xs) (y:ys) (z:zs) = mySort ( uniqList ( [x] ++ xs ++ [y] ++ ys ++ [z] ++ zs ) )

