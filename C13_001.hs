import  Control.Monad.Writer 

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

-- applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)
-- applyLog :: (a,[c])    -> (a -> (b,[c]))    -> (b,[c])   -- Now the [c] can be list of any type, not just a [Char]
-- applyLog (x,log) f = let (y,newLog) = f x
--                      in (y,log ++ newLog)

-- ghci> (3, "Smallish gang.") `applyLog` isBigGang  
-- (False,"Smallish gang.Compared gang size to 9")  

-- ghci> ("Tobin","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))  
-- (5,"Got outlaw name.Applied length.")  

                
-- after adding a (Monoid m) constrain and changing "++" with "mappend", the function will work for any instance of Monoid, not just [c] 
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x 
                                      in (y,log `mappend` newLog)  
-- ghci>  applyLog ( 3 , pack [97,98,99] ) (\x -> ( x + 2, pack [100,101]))  
-- (5,"abcde")

-- ghci>  applyLog ( 3 , Just"abc" ) (\x -> ( x + 2, Just"de"))  
-- (5,Just "abcde")


-- ****************************************************************


-- newtype Sum a =  Sum { getSum :: a }  deriving (Eq, Ord, Read, Show, Bounded)

-- -- Missing Monoid instance implementation for "Sum a"

-- addDrink :: String -> (String,Sum Int)  
-- addDrink "beans" = ("milk", Sum 25)  
-- addDrink "jerky" = ("whiskey", Sum 99)  
-- addDrink _ = ("beer", Sum 30)  
-- -- ghci> ("beans", Sum 10) `applyLog` addDrink  
-- -- ("milk",Sum {getSum = 35})  


-- ****************************************************************

type Product = String
type Price = Int
type ShoppingList = String
-- getProduct :: String -> (Log, Price)
getProduct x 
                |x=="a" = ("added a, " , 10)
                |x=="b" = ("added b, " , 20)
                |otherwise = ("added "++ x , 30)

addToshoppingList (order, shoppingList, prevTotal) f = let (addedProduct,price) = f order 
                                              in (shoppingList `mappend` addedProduct , price + prevTotal)  
-- ghci>  addToshoppingList ("something" , "added a, added b, ",30) getProduct
-- ("added a, added b, added something",60)
-- ghci>  addToshoppingList ("a" , "added a, added b, ",30) getProduct
-- ("added a, added b, added a, ",40)


-- ****************************************************************


appendNumber :: Int -> Writer [String] Int  
appendNumber x = writer (x, ["Got number: " ++ show x])  


multWithLog = runWriter(  appendNumber 3 >>= \a -> tell ["Waiting for one more Nr"] >>= \x ->  appendNumber 5   >>= \b -> return (a*b) ) --NOTICE that "x" is just a dummy value which = () its NOT an Int!


multWithLog_DoNotatoin =  runWriter(
                                    do
                                        a <- appendNumber 3
                                        x <- tell ["Gonna multiply these two"]  --NOTICE that "x" is just a dummy value which = () its NOT an Int!
                                        b <- appendNumber 5
                                        return (a*b)
                                    )

-- ghci>  multWithLog
-- (15,["Got number: 3","Got number: 5"])


-- ****************************************************************


gcd' :: Int -> Int -> Writer [String] Int    
gcd' a b  
    | b == 0 = 
        --  instead of "do-notation" we could write:  Writer (a, ["Finished with " ++ show a])  
        do  
        tell ["Finished with " ++ show a]   -- Writer (     ()     , "Finished with theValueOfa" )
        return a                            -- Writer (theValueOfa ,      ""                     )
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)  

-- Notice the use of "mapM_ putStrLn" to print the strings 
-- ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)  
-- 8 mod 3 = 2  
-- 3 mod 2 = 1  
-- 2 mod 1 = 0  
-- Finished with 1  

