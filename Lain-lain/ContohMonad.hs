-- Tidak perlu diaktifkan, sudah tersedia di Prelude (built-in library)
-- data Maybe a = Nothing
--             | Just a
-- lookup :: a -> [(a, b)] -> Maybe b

import Data.List



animalFriends :: [(String, String)]
animalFriends = [ ("Pony", "Lion")
                , ("Lion", "Manticore")
                , ("Manticore", "Unicorn")
                , ("Unicorn", "Lepricon") ]

-- Does Pony's friend have a friend in animalMap?
animalFriendLookup :: [(String, String)] -> Maybe String
animalFriendLookup animalMap =
  case lookup "Pony" animalMap of
       Nothing -> Nothing
       Just ponyFriend ->
         case lookup ponyFriend animalMap of
              Nothing -> Nothing
              Just ponyFriendFriend -> 
                case lookup ponyFriendFriend animalMap of
                  Nothing -> Nothing
                  Just ponyFriendFriendFriend -> 
                    case lookup ponyFriendFriendFriend animalMap of
                        Nothing -> Nothing
                        Just friend -> Just friend

monadicFriendLookup :: [(String, String)] -> Maybe String
monadicFriendLookup animalMap =
  lookup "Pony" animalMap
  >>= (\ponyFriend -> lookup ponyFriend animalMap
  >>= (\pony2ndFriend -> lookup pony2ndFriend animalMap
  >>= (\friend -> Just friend)))

sugaryFriendLookup :: [(String, String)] -> Maybe String
sugaryFriendLookup animalMap = do
  ponyFriend    <- lookup "Pony" animalMap
  ponyFriend'   <- lookup ponyFriend animalMap
  ponyFriend''  <- lookup ponyFriend' animalMap
  return ponyFriend''

test0 = [(x,y)| x<-[1,2,3],y<-[4,5]]

test1 = do
        x <- [1,2,3]
        y <- [4,5]
        return (x,y)

test2 = ([1,2,3] >>= \x -> [4,5] >>= \y -> return (x,y))

-- instance  Monad []  where	    
--    m >>= k	=  concat (map k m)	    
--    return x	=  [x]	    
--    fail x	=  [ ]

test3 = ([1,2,3] >>= (\x -> [4,5] >>= (\y -> return (x,y))))

-- tahap 1
test_1 = concat (map (\x -> [4,5] >>= (\y -> return (x,y))) 
                    [1,2,3])

-- tahap 2
-- [4,5] >>= (\y -> return (x,y))
-- concat (map (\y -> return (x,y) [4,5] )
test_2 = concat (map (\x -> concat (map (\y -> return (x,y)) [4,5]))
                     [1,2,3])

-- tahap 3
-- return (x,y) = [(x,y)]
test_3 = concat (map (\x -> concat (map (\y -> [(x,y)]) [4,5] )  ) 
                     [1,2,3])

test4 = concat (map (\x -> (concat (map (\y -> [(x,y)])
                                        [4,5])))
                    [1,2,3])

type Sexpr = String

-- naive generation of unique symbol
transformStmt :: Sexpr -> Int -> (Sexpr, Int)
transformStmt expr counter = (newExpr, counter+1)
  where newExpr = "(define " ++ var ++ " " ++ expr ++ ")"
        var = "tmpVar" ++ (show counter)

primas = sieve [2..]
  where sieve (x:xs) = x : sieve [y | y<-xs, y `mod` x /=0]

satu = 1 : satu