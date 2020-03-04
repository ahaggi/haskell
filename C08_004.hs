import qualified Data.Map as Map

data TrafficLight = Red | Yellow | Green  

-- class Eq where
--     (==) :: a -> a -> Bool 
--     (/=) :: a -> a -> Bool
--"==" is NOT defined in terms of "/=" and vice versa

-- instance Eq TrafficLight where  
--     Red == Red = True  
--     Red /= Red = false  
--     Green == Green = True  
--     Green /= Green = false
--     Yellow == Yellow = True  
--     Yellow /= Yellow = True  
--     _ == _ = False
--     _ /= _ = False


-- class Eq a where  
--     (==) :: a -> a -> Bool  
--     (/=) :: a -> a -> Bool  
--     x == y = not (x /= y)  --"==" is defined in terms of "/=" and vice versa
--     x /= y = not (x == y)  


instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  --catch-all pattern: if none of the previous combinations, then two lights aren't equal

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  



class OurOwnTypeClass a where
    somefunc :: a -> String

instance OurOwnTypeClass Int where 
    somefunc 0 = "It's a zero"
    somefunc _ = "it's a num"

    -- ghci>   somefunc (0::Int)
    -- "It's a zero"
    


-- ******************************************************* --
--implement JavaScript-ish behavior if(obj), where obj not necessary a Bool value


class YesNo a where  
    yesno :: a -> Bool  

-- if(Zero) is false, otherwise true
instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True  
    -- ghci>   yesno (0::Int)
    -- False
    
-- if(Empty lists) is false, otherwise true
instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True  

-- if(true) is true, otherwise false
instance YesNo Bool where  
    yesno = id  -- id is just a standard library function that takes a parameter and returns the same thing     

instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False  

    -- ghci> yesno $ length []  
    -- False  
    -- ghci> yesno "haha"  
    -- True  
    -- ghci> yesno ""  
    -- False  
    -- ghci> yesno []  
    -- False  
    -- ghci> yesno [0,0,0]  
    -- True  
    -- ghci> yesno $ Just 0  
    -- True  
    -- ghci> yesno True  
    -- True  
    -- ghci>  if(yesno "haha") then 2 else 3
    -- 2

-- ******************************************************* --



-- instance Functor Maybe where               --Duplicate instance declarations, Defined in ‘GHC.Base’
--     fmap f (Just x) = Just (f x)
--     fmap f Nothing = Nothing

    -- If we map over Nothing, then just return a Nothing. Just like if we map over an empty list, we get back an empty list. 
    -- If we map over a single value packed up in a Just, then we apply the function on the contents of the Just.

    -- ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")  
    -- Just "Something serious. HEY GUYS IM INSIDE THE JUST"  
    -- ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing  
    -- Nothing  
    -- ghci> fmap (*2) (Just 200)  
    -- Just 400  

-- ******************************************************* --

-- instance Functor (Either a) where          --Duplicate instance declarations, Defined in ‘GHC.Base’
--     fmap f (Left x) = Left x
--     fmap f (Right x) = Right (f x)


    -- ghci>  fmap ("aaa "++) (Left "bbb")
    -- Left "bbb"
    -- ghci>  fmap ("aaa "++) (Right "bbb")
    -- Right "aaa bbb"
