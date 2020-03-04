addThree :: (Num a) => a -> a -> a -> a  
addThree x y z = x + y + z  

addThree' :: (Num a) => a -> a -> a -> a  
addThree' = \x -> \y -> \z -> x + y + z  

zipWithLammbda = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]


-- recursively combining the results of combining all but the last element with the last one, (called a left fold). 
-- tail recursion
foldLeft' _ acc [] = acc
foldLeft' f acc (x:xs) = foldLeft' f (f acc x) xs
-- ((((1) + 2) + 3) + 4) + 5
-- ghci>  foldLeft' (\acc x -> acc + x) 0 [1,2,3,4] --> 10
-- ghci>  foldLeft' (\acc x -> acc - x) 0 [1,2,3,4] --> -10
-- ghci>  foldLeft' (\acc x -> acc * x) 0 [1,2,3,4] --> 0
-- ghci>  foldLeft' (\acc x -> acc + x) 0 []        --> 0

-- recursively combining the first element with the results of combining the rest (called a right fold) 
foldRight' _ acc [] = acc 
foldRight' f acc (x:xs) = f x (foldRight' f acc xs)                           -- normal recursion
-- 1 + (2 + (3 + (4 + (5))))

-- foldRight' f acc xs = foldLeft' f acc (foldLeft' (\acc x -> x: acc) [] xs) -- tail recursion
-- foldRight' f acc xs = foldLeft' f acc (reverse xs)                         -- tail recursion


-- sum' xs = foldLeft' (+) 0 xs
sum' xs = foldLeft' (\acc x -> acc + x) 0 xs

elem' y xs = foldLeft' (\acc x -> if x == y then True else acc) False xs  

-- map function with foldLeft'. The accumulator will be a list, we'll be accumulating the mapped list element by element.
-- From that, it's obvious that the starting element will be an empty list.
map_foldRight' f xs = foldRight' (\acc x  -> f x : acc) [] xs

map_foldLeft' f xs = foldLeft' (\acc x  -> acc ++ [f x]) [] xs  -- Notice how we append the (f x) to the end of the list
                                                                -- if we use foldLeft' (\acc x  -> f x : acc) [] xs
                                                                -- the res list will be reversed


-- scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator states in the form of a list. There are also scanl1 and scanr1, which are analogous to foldl1 and foldr1
scanLeft' _ acc [] = []
scanLeft' f acc (x:xs) = f acc x : scanLeft' f (f acc x) xs
-- scanLeft' (\ acc x -> (x + acc) * 2 ) 0 [1,2,3,4]
-- [2,8,22,52]
-- ghci>  scanLeft' (+) 0 [1,2,3,4]
-- [1,3,6,10]



-- TODO scanRight'


fn x = let creatMaxList = zipWith max [1,2,3,4,5] x
           multiWith3 =  map (*3) creatMaxList
           getProduct = product multiWith3
     in  replicate 100 getProduct

--  fn [4,5,6,7,8]
-- is more readable than     

-- let fn x = replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5]   $ x  
-- let fn x = replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] x)))
-- let fn = replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5]

-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
-- (replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5])   [4,5,6,7,8]
-- replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5]   $ [4,5,6,7,8]




-- Read C10 
solveRPN :: (Num a, Read a) => String -> a  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction xs numberString = read numberString:xs  


