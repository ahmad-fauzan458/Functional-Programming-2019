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

-- What is the type and effect of the following function?
-- • \n -> iter n succ
-- succ is the successor function, which increases a value by one:
-- Prelude> succ 33
-- 34
-- 
-- Tipenya adalah (Integral a, Num b, Enum b) => a -> (b -> b) -> b -> b
-- Tipe dari hasilnya adalah (Num b, Enum b) => b
-- Efeknya adalah, iter n succ m akan melakukan fungsi succ m sebanyak n kali
-- Misal iter 5 succ 10 akan menghasilkan 15

-- (*) How would you define the sum of the squares of the natural numbers 1
-- to n using map and foldr?
sumSquares n = foldr (\x acc -> acc + x^2) 0 [1..n]

-- How does the function
mystery xs = foldr (++) [] (map sing xs)
    where sing x = [x]
-- behave?
-- Fungsi tersebut bertindak seperti fungsi identitas
-- Contoh : mystery [1,2,3] = [1,2,3]
-- Jika kita perhatikan, map sing xs memetakan sebuah list of e menjadi list of (list of e)
-- Misal map sing [1,2,3] = [[1], [2], [3]]
-- Kemudian untuk setiap x elemen map sing xs akan dikonkat dengan acc
-- dimana acc adalah hasil dari operasi konkat x sebelumnya dengan acc sebelumnya

-- acc pertama (sebelum ada pengambilan x) adalah []
-- pengambilan x dimulai dari elemen paling kanan
-- [3] ++ [] = [3]
-- [2] ++ [3] = [2,3]
-- [1] ++ [2,3] = [1,2,3]
