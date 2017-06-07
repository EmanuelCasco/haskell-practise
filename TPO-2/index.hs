elem' :: Eq a => a -> [a] -> Bool
elem' n = foldl (\a x -> a || x == n) False 

map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f xs = foldr (\y ys -> f y : ys) [] xs

filter' :: (a-> Bool) -> [a] -> [a]
filter' f  = foldr (\x xs -> cumple f x ++ xs) []
     where cumple f x | f x = [x] | otherwise = []  

maximum' :: Ord a => [a] -> a
maximum' = foldl1 max 

minimum' :: Ord a => [a] -> a
minimum' = foldl1 min 

any' :: (a->Bool) -> [a] -> Bool
any' f = foldl (\x xs -> x || f xs) False 

last' :: [a] -> a
last' = foldl1 (\x xs -> xs) 

length' :: [a] -> Int
length' = foldl (\a x -> a + 1) 0