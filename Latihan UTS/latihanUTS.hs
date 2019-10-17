max' :: (Ord a) => a -> a -> a
max' a b = if (a > b) then a else b

maxThree :: (Ord a) => a -> a -> a -> a
maxThree a b c = max' a (max' b c)