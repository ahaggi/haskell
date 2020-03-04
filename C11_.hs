-- data type that looks much like Maybe a, only the Just part holds two fields instead of one. 
-- The first field in the CJust value constructor will always have a type of Int, and the second field is of type a
data CMaybe a = CNothing | CJust Int a deriving (Show)  

-- Notice that this type can't be a functor, because of how we implemented the fmap
instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x)  

-- To examine why our type CMaybe can't be a functor, first we can test if (Maybe a) is a functor

    -- ghci>  fmap id (Just "abc")
    -- Just "abc"                   #### the 1.st low holds true


    -- ghci>  fmap (++ " def") (Just "abc")
    -- Just "abc def"
    -- ghci>  fmap (++ " ghi") (Just "abc def ")
    -- Just "abc def  ghi"

    -- ghci>  fmap ((++ " ghi").(++ " def")) (Just "abc")
    -- Just "abc def ghi"           #### the 2.nd low holds true



-- Now let's do the same with CMaybe 
    -- ghci>  fmap id (CJust 1 "abc")
    -- CJust 2 "abc"                #### the 1.st low Does NOT holds true,, CJust 1 "abc" /= CJust 2 "abc"

    -- ghci>  fmap (++ " def") (CJust 1 "abc")
    -- CJust 2 "abc def"
    -- ghci>  fmap (++ " ghi") (CJust 2 "abc def")
    -- CJust 3 "abc def ghi"

    -- ghci>   fmap ((++ " ghi").(++ " def")) (CJust 1 "abc")
    -- CJust 2 "abc def ghi"        #### the 2.nd low Does NOT holds true,, CJust 3 "abc def ghi"  /= CJust 2 "abc def ghi"



-- ****************************************************************************************************************************************

-- ghci>  pure (*) <*> [1,2,3] <*> [2]     -- pure (*) ==>  [(*)]
-- ghci>  [(*)] <*> [1,2,3] <*> [2]        -- [(*)] <*> [1,2,3]    ==> [(*1) ,(*2) ,(*3)]
-- ghci>  [(*1) ,(*2) ,(*3)] <*> [2]
-- ghci>  (*) <$> [1,2,3]    <*> [2]       -- (*) <$> [1,2,3]      ==> [(*1) ,(*2) ,(*3)]
-- ghci>  fmap (*) [1,2,3]   <*> [2]       -- fmap (*) [1,2,3]     ==> [(*1) ,(*2) ,(*3)]


-- pure f <*> x                         fmap f x                f <$> x 
-- pure (*2) <*> [1,2,3]           fmap (*2) [1,2,3]        (*2) <$> [1,2,3]
-- <$> is just fmap as an infix operator.

-- [(*)] <*> [1,2,3] <*> [10,100]             fmap (*) [1,2,3] <*> [10,100,1000]             (*) <$> [1,2,3] <*> [10,100,1000]          [ x*y | x <- [1,2,3], y <-[10,100,1000]]
--                                                                     [10,100,1000,20,200,2000,30,300,3000]



-- (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
-- this means
--     ...................... (5+3)  (5*2)  (5/2)
--     if k is (\x y z -> [x,y,z])
--     so k (5+3)  (5*2)  (5/2) equals

-- [8.0,10.0,2.5]



-- ****************************************************************************************************************************************



-- (:) <$> Just 3 <*> Just [4]   ==>  Just 3:[4] 

sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs

-- ghci>  sequenceA' [Just 1, Just 2, Just 3, Just 4]
-- Just [1,2,3,4]

-- Not important but:
-- ghci>  sequenceA' [Just 1, Just 2, Just 3, Nothing]
-- Nothing
-- ghci>  sequenceA' [[1,2,3],[4,5,6],[3,4,4],[]]  
-- []

-- Read the section Important difference between the many Applicative instances' implementations for <*>, we will use the our own function  sequenceA'
-- ghci>  sequenceA' [[1,2,3],[4,5,6]]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
-- ghci>  sequenceA' [Just 3, Just 4]
-- ghci> Just [3,4]
