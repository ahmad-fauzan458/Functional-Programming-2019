-- (*) Define the length function using map and sum
length' xs = sum $ map ($1) xs
