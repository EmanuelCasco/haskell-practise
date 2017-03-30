esPeorSegun :: Ord a => t -> t -> (t -> a) -> Bool
esPeorSegun x y f = (f x) < (f y)
