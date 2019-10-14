-- (*) Define the length function using map and sum
length' xs = sum $ map (\x -> 1) xs 

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

-- (*) If id is the polymorphic identity function, defined by id x = x, explain the
-- behavior of the expressions
--
-- id . f = f, karena mengaplikasikan id ke hasil dari f tidak mengubah apa apa
-- f . id = f, karena pemrosesan argument f oleh id tidak mengubah nilai dari argument tersebut
-- id f = f, karena mengaplikasikan id ke sesuatu tidak mengubah sesuatu tersebut
--
-- If f is of type Int -> Bool, at what instance of its most general type a ->
-- a is id used in each case?
--
-- Pada id . f, Bool -> Bool
-- Pada f . id, Int -> Int
-- Pada id f, (Int -> Bool) -> (Int -> Bool)

-- Define a function composeList which composes a list of functions into a single
-- function. You should give the type of composeList, and explain why the function
-- has this type. What is the effect of your function on an empty list of functions?

composeList :: [(a -> a)] -> (a -> a)
-- composeList memiliki type ini karena memiliki parameter list of function dan akan menghasilkan single function
-- (a -> a) merupakan sebuah fungsi yang menerima a dan menghasilkan a
-- seperti itu karena untuk bisa melakukan komposisi, misal pada x . y, maka tipe parameter yang diterima x harus sama dengan 
-- tipe output dari fungsi y
composeList [] = id
composeList (x:xs) = x . composeList xs 
-- pada sebuah empty list, maka fungsi composeList yang saya buat akan menghasilkan fungsi id, hal itu digunakan sebagai base case

-- (*) Define the function flip :: (a -> b -> c) -> (b -> a -> c)
-- which reverses the order in which its function argument takes its arguments.
-- The following example shows the effect of flip:
-- Prelude> flip div 3 100
-- 33

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = (\x y -> f y x)

-- 2 (*). List Comprehensions and Higher-Order Functions

-- Can you rewrite the following list comprehensions using the higher-order
-- functions map and filter? You might need the function concat too.
-- 1. [ x+1 | x <- xs ]
-- 2. [ x+y | x <- xs, y <-ys ]
-- 3. [ x+2 | x <- xs, x > 3 ]
-- 4. [ x+3 | (x,_) <- xys ]
-- 5. [ x+4 | (x,y) <- xys, x+y < 5 ]
-- 6. [ x+5 | Just x <- mxs ]

questionOne xs = map (+1) xs 

questionTwo [] _ = []
questionTwo (x:xs) ys = (map (+x) ys) ++ (questionTwo xs ys)

questionThree xs = map (+2) (filter (\x -> x > 3) xs)

questionFour xys = map (\(x,y) -> x+3) xys

questionFive xys = map (\(x,y) -> x+4) (filter (\(x,y) -> x+y < 5) xys) 

questionSix mxs = map (\(Just x) -> x+5) mxs

-- Can you it the other way around? I.e. rewrite the following expressions as list
-- comprehensions.
-- 1. map (+3) xs
-- 2. filter (>7) xs
-- 3. concat (map (\x -> map (\y -> (x,y)) ys) xs)
-- 4. filter (>3) (map (\(x,y) -> x+y) xys)

questionOne' xs = [x+3 | x <- xs]

questionTwo' xs = [x | x <- xs, x > 7]

questionThree' xs ys = [(x,y) | x<-xs, y<-ys]

questionFour' xys = [ x+y | (x,y) <- xys, x+y > 3]


-- 3
-- * 3
-- Generating Lists

listOfLength :: Int -> Gen a -> Gen [a]
listOfLength n gen = sequence [ gen | i <- [1..n] ]

pairsOfEqualLengthLists :: Gen a -> Gen ([a],[a])
pairsOfEqualLengthLists gen =
  do n <- choose (0,100)
     xs <- listOfLength (abs n) gen
     ys <- listOfLength (abs n) gen
     return (xs,ys)
