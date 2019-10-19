doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x*2
doubleBigNumber x = if x > 100 then x*2 else x 

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)

bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs

bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | d <= 18.5 = "You're underweight, you emo, you!"  
    | d <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | d <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where d = weight / height ^ 2 

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [d w h | (w, h) <- xs]  
    where d weight height = weight / height ^ 2 
    
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname 

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  

primes = sieve [2,3..100] where
    sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x > 0]

trip = [(x,y,z) |  z <- [5..], y <- [4..z], x <- [3..y], x^2+y^2 == z^2]

-- ($) :: (a -> b) -> a -> b  
-- f $ x = f x 

-- Example without $ function
sqrtSum = sum (map sqrt [1..130])

-- Exampe with $ function
sqrtSum' = sum $ map sqrt [1..130]

-- The foldl1 and foldr1 functions work much like foldl and foldr, 
-- only you don't need to provide them with an explicit starting value. 
-- They assume the first (or last) element of the list to be the starting 
-- value and then start the fold with the element next to it. With that in 
-- mind, the sum function can be implemented like so: sum = foldl1 (+). 
-- Because they depend on the lists they fold up having at least one element,
-- they cause runtime errors if called with empty lists

-- Example foldr1
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

-- Example foldl1
last' :: [a] -> a  
last' = foldl1 (\_ x -> x) 

-- scanl and scanr are like foldl and foldr, only they report all the 
-- intermediate accumulator states in the form of a list. There are also
-- scanl1 and scanr1, which are analogous to foldl1 and foldr1.

exampleScanl =  scanl (+) 0 [3,5,2,1]  
-- exampleScanl = [0,3,8,10,11]  
exampleScanr = scanr (+) 0 [3,5,2,1]  
-- exampleScanr =[11,8,3,1,0]  
