module C05C06 (collatzSeqGT15 , quicksort ) where


-- Tail vs Normal recursion
-- normal recursion
addingN 0 = 0
addingN n = n + (addingN (n-1))
-- addingN 5 = 
--             5 + (addingN 4 )
--             5 + (        4 + (addingN 3))
--             5 + (        4 + (        3 + (addingN 2)))
--             5 + (        4 + (        3 + (        2 + (addingN 1))))
--             5 + (        4 + (        3 + (        2 + (        1 + addingN 0))))
--             5 + (        4 + (        3 + (        2 + (        1 +         0))))
--             5 + (        4 + (        3 + (        2 + (    1    ))))
--             5 + (        4 + (        3 + (    3    )))
--             5 + (        4 + (    6    ))
--             5 + (    10    )
--             15

-- tail  recursion
addingN' 0 acc = acc 
addingN' n acc = addingN' (n-1) (acc + n)

-- addingN' 5 0 = 
--             addingN' 4 5
--             addingN' 3 9
--             addingN' 2 12
--             addingN' 1 14
--             addingN' 0 15
--             15

-- foldRight' f acc (x:xs) = f x (foldRight' f acc xs) -- normal recursion

-- foldLeft' f acc (x:xs) = foldLeft' f (f acc x) xs   -- tail  recursion



fb 0 = 0
fb 1 = 1
fb n = fb (n-1) + fb (n-2)


fb' n = fibFn' 0 1 n
fibFn' a b 0 = a
fibFn' a b n = fibFn' b (a+b) (n-1)


fb'' n = last $ take (n+1) (fibFn'' 0 1)
fibFn'' a b = a: fibFn'' b (a+b)
-- f' a b = a: f' b (a+b)




maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  


replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  


take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

-- take' :: (Ord t, Num t, Eq a) => t -> [a] -> [a]
-- take' _ [] = []  
-- take' n all@(x:xs)  
--     | n <= 0   = []  
--     | xs == [] = [x]
--     | otherwise = x : take' (n-1) xs

reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  

-- zip [1,2,3] [2,3] returns [(1,2),(2,3)]
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  


-- elem' :: (Eq a) => a -> [a] -> Bool  
-- elem' a [] = False  
-- elem' a (x:xs)  
-- implement!!


-- quicksort :: (Ord a) => [a] -> [a]  
-- quicksort [] = []  
-- quicksort (x:xs) =   
--     let smallerSorted = quicksort [a | a <- xs, a <= x]  
--         biggerSorted = quicksort [a | a <- xs, a > x]  
--     in  smallerSorted ++ [x] ++ biggerSorted  

--    let smallerSorted = quicksort (filter (<=x) xs)  
--        biggerSorted  = quicksort (filter (>x) xs)   

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) = smallerSorted ++ [x] ++ biggerSorted 
    where smallerSorted = quicksort [a | a <- xs, a <= x]  
          biggerSorted = quicksort [a | a <- xs, a > x]


-- takes a function and two lists as parameters and then joins the two lists by applying the function between corresponding elements. 
-- ghci>  zipWith (+) [1,2,3][5,6,7,8]
-- [6,8,10]
zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- ghci> zipWith' (*) [1,2,3] [3,2,2]
-- [3,4,6]

-- ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]  
-- [[3,4,6],[9,20,30],[10,12,12]]  


-- takes a function f and returns a function that is like our original function f, only the first two arguments are flipped.
-- The original function f must at least have 2 param, but it can have more than 2 
flip' f = g  
    where g x y = f y x  


--map takes a function and a list and applies that function to every element in the list, producing a new list.
map' _ [] = []
map' f (x:xs) = f x : map f xs 


-- filter is a function that takes a predicate and a list and then returns the list of elements that satisfy the predicate. 
filter' _ [] = []
filter' p (x:xs)
        | p x       = x : filter' p xs  
        | otherwise = filter' p xs


-- takes a predicate and a list and then goes from the beginning of the list and returns its elements while the predicate holds true. 
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) 
        | p x       = x: takeWhile' p xs
        | otherwise = []


-- find the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0  
-- Because we only end up using the head of the filtered list, it doesn't matter if the filtered list is finite or infinite. 

collatzSeq :: (Integral t) => t -> [t]
collatzSeq x
    | x==1 = []
    | odd x = a : collatzSeq a
    | otherwise = b : collatzSeq b
    where a = (x*3) + 1
          b = x `div` 2



-- ******************************* Important *******************************

-- map  + filter             equevalent to     comprehensions
-- curried, parital applied  equevalent to     Lambda


-- takeWhile vs filter: filter doesn't work on infinite lists, but takeWhile does. 


-- Thanks to Haskell's laziness, even if you map something over a list several times and filter it several times, it will only pass over the list once.
    -- length (filter (> 15) (map length (map collatzSeq [1..100]))),,,  Has complexity of N(100)


-- Function application with $:     f $ g x    vs      f(g x)


-- Function composition                 vs                    lambda                     vs    using whereBinding 
------------------------------------------------------------------------------------------------------------------
-- filter ((> 15) . length) someList    |    filter (\xs -> length xs > 15) someList     |    filter isLong
--                                      |                                                |                
-- map (negate . abs) [5,-3,-6]         |        map (\x -> negate (abs x))              |
------------------------------------------------------------------------------------------------------------------



    -- for all starting numbers between 1 and 100, how many collatzSeqs have a length greater than 15
collatzSeqGT15 = sum [ 1 | x <- [1..100] , (length (collatzSeq x)) > 15]


collatzSeqGT15_' =  length (filter (> 15) (map length (map collatzSeq [1..100])))
                -- length $ filter (> 15) $ map length $ map collatzSeq [1..100]   -- function application with $ 

collatzSeqGT15__' = length (filter ((> 15) . length) (map collatzSeq [1..100]))


collatzSeqGT15'' = length (filter isLong (map collatzSeq [1..100]))  
                    where isLong xs = length xs > 15

collatzSeqGT15_'' = length (filter (\xs -> length xs > 15) (map collatzSeq [1..100]))

collatzSeqGT15Composition    =  length  . filter (> 15) . map length . map collatzSeq $ [1..100]
collatzSeqGT15Composition'   = (length  . filter (> 15) . map length . map collatzSeq)  [1..100]
collatzSeqGT15Composition'' x = (length  . filter (> 15) . map length . map collatzSeq)  x
collatzSeqGT15Composition''_ x =  length  . filter (> 15) . map length . map collatzSeq  $ x
 

-- *************************************************************************

