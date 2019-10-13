-- (*) Define the length function using map and sum
length' xs = sum $ map ($1) xs

-- (*) What does map (+1) (map (+1) xs) do? Can you conclude anything in
-- general about properties of map f (map g xs), where f and g are arbitrary
-- functions?
addByTwo xs = map (+1) (map (+1) xs)
-- map (+1) (map (+1) xs) menambahkan angka 2 pada setiap elemen di list xs
-- Contoh : map (+1) (map (+1) [0,1,2]) = [2,3,4]

-- • iter n f x = f (f (... (f x)))
-- where f occurs n times on the right-hand side of the equation. For instance, we
-- should have
-- iter 3 f x = f (f (f x))
-- and iter 0 f x should return x.
iter :: (Integral a) => a -> (b -> b) -> b -> b
iter 0 _ x = x
iter n f x = f $ iter (n-1) f x
