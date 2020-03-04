
-- takes param of type t and return value of type t,, as long as t implements the type classes Eq and Num
-- => symbol is called a class constraint, the constraints are (as long as t impl ..)
-- Eq typeclass provides an interface for testing for equality. Any type where it makes sense to test for equality between two values of that type should be a member of the Eq class
factorial_ :: (Eq t, Num t) => t -> t
-- factorial_ :: (Integral a) => a -> a  Because members of Integral are also members of Eq and Num
factorial_ x = if x == 0 then 1 else x * factorial_ (x-1)

-- factorial without "if expression"
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1) -- fallBack definition

-- addVectors without pattern matching
-- addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
-- addVectors a b = (fst a + fst b, snd a + snd b)      

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  


fst_ :: (a, b, c) -> a  
fst_ (x, _, _) = x  
  
snd_ :: (a, b, c) -> b  
snd_ (_, y, _) = y  
  
trd_ :: (a, b, c) -> c  
trd_ (_, _, z) = z  

-- *************************************************************************************
-- *************************************************************************************

-- Notice that if you want to bind to several variables (even if one of them is just _ and doesn't actually bind at all), we have to surround them in parentheses. 
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  -- where bindings aren't shared across function bodies of different patterns. We can't use "z" here.. Variable not in scope: z
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show z -- We can use z here, becuase "where bindings" 
                                                                                        where z=y  
-- Where is NOT an expression
tellWhere xs = "The list " ++ what xs
                where what [] = "is empty"
                      what (x:[]) = "has one element: " ++ show x 
                      what (x:y:[]) = "has two elements: " ++ show x ++ " and " ++ show y
                      what (x:y:_) = "is long. The first two elements are: " ++ show x ++ " and " ++ show y


-- let is an expression
tellLet xs = "The list " ++ let what [] = "is empty"
                                what (x:[]) = "has one element: " ++ show x 
                                what (x:y:[]) = "has two elements: " ++ show x ++ " and " ++ show y
                                what (x:y:_) = "is long. The first two elements are: " ++ show x ++ " and " ++ show y
                            in what xs

getProduct x 
            |x=="a" = ("added a, " , 10)
            |x=="b" = ("added b, " , 20)
            |otherwise = ("added "++ x , 30)

addToshoppingList (order, shoppingList, prevTotal) f = let (addedProduct,price) = f order 
                                              in (shoppingList `mappend` addedProduct , price + prevTotal)  


-- case is an expression
tellCase xs = "The list is " ++ case xs of [] -> "is empty"
                                           (x:[]) -> "has one element: " ++ show x 
                                           (x:y:[]) -> "has two elements: " ++ show x ++ " and " ++ show y
                                           (x:y:_) -> "is long. The first two elements are: " ++ show x ++ " and " ++ show y


-- Notice There are two places guards are allowed: 
--              function definitions and case expressions. In both contexts, guards appear after a pattern and before the body
-- fun x = something  
-- fun x "guards" = something
-- fun x
        -- | 0 = something0 
        -- | otherwise = something1 

-- case (Bool, Int) of
--     case1 -> something
--     case2 -> somethingElse

-- case (Bool, Int) of
--     case1 "guards" -> something
--     ...
-- it's easier to rewrite this function with pattern matching, but this is just to show using "guards" with "case..of"
ff someElem = case someElem of
                            (True, nr ) 
                                | nr== 0 -> 1.1
                                | nr== 1 -> 1.2
                                | otherwise -> 1.3
                            (False, _ )  -> 2.0

-- this function has 2 cases where the 2.nd case has 3 guards
tellCase' xs = "The list is " ++ case xs of [] -> "is empty"
                                            xs  | (length xs)==1 -> "has one element: " ++ show (xs !! 0)
                                                | (length xs)==2 -> "has two elements: " ++ show (xs !! 0) ++ " and " ++ show (xs !! 1) 
                                                | otherwise ->"is long. The first two elements are: " ++ show (xs !! 0) ++ " and " ++ show (xs !! 1)


-- *************************************************************************************


length2' :: (Num b) => [a] -> b  
length2' xs = case xs of [] -> 0  
                         (_:xs) -> 1 + length2' xs


length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  

-- *************************************************************************************
-- *************************************************************************************

--showElm [] = ""
--showElm (x:xs)


bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  -- Note that there's no = right after the function name and its parameters
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  

                          
bmiTell_ :: (RealFloat a) => a -> a -> String  
bmiTell_ weight height   -- Note that there's no = right after the function name and its parameters
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  
-- Notice that all the names are aligned at a single column. If we don't align them nice and proper, Haskell gets confused because then it doesn't know they're all part of the same block.

--      where bmi = weight / height ^ 2  
--      (skinny, normal, fat) = (18.5, 25.0, 30.0)  






doubelxy :: Num a => a -> a -> a
doubelxy x y = (dbl x) + ( dbl y) 
            where dbl z = 2*z


-- initials :: String -> String -> String  
initials :: [Char] -> [Char] -> [Char]
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname
-- Alt we can use pattern matching/ destructuring
initials_ (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."  


-- a function that takes a list of weight-height pairs and returns a list of BMIs.
--   calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis :: Fractional a => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  


cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  