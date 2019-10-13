import Data.List

perms [] = [[]]
perms ls = [ x:sisa | x <- ls, sisa <- perms (ls \\ [x])]