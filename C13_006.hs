import  Control.Monad
import  Control.Monad.Writer 


keepSmall :: Int -> Writer [String] Bool  
keepSmall x = writer ( (x<5)  , if(x<5) 
                                then ["Keeping " ++ show x] 
                                else  [show x ++ " is too large, throwing it away"]   ) 

                                -- The same but with tell + return
keepSmall' :: Int -> Writer [String] Bool  
keepSmall' x  
        | x < 5 = do  
            tell ["Keeping " ++ show x]  
            return True  
        | otherwise = do  
            tell [show x ++ " is too large, throwing it away"]  
            return False  
-- ghci>  mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [1,2,3,4,5,6,7,8,9]



sumSmalls :: Int -> Int -> Maybe Int  
sumSmalls acc x  
    | x > 5     = Just (acc)  
    | otherwise = Just (acc + x)  
-- ghci>  foldM sumSmalls 0  [1,2,3,4,9,5,6,7,8]
-- Just 15



binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 5     = Nothing  
    | otherwise = Just (acc + x)  
-- ghci>  foldM binSmalls 0 [1,2,3,4,5,6,7,8,9]
-- Nothing


-- ********************************************************************************************

-- reads function is like read, only it returns a list with a single element in case of a successful read. 
-- If it fails to read something, then it returns an empty list. 

-- ghci>  reads "4abc" ::[(Int, String)]
-- [(4,"abc")]
-- ghci>  reads "abc4" ::[(Int, String)]
-- []
-- ghci>  reads "abc4" ::[(Int, String)]
-- []
-- ghci>  reads "4" ::[(Int, String)]
-- [(4,"")]


readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of
                            [(x,"")] -> Just x  
                            _ -> Nothing  


-- function that takes as its parameter a string that contains a RPN expression, like solveRPN "10 4 3 + 2 * -" and gives us back its result
foldingFunction ::  [Double] -> String -> Maybe [Double]  
foldingFunction (x:y:ys) "*" = return((x * y):ys)  
foldingFunction (x:y:ys) "+" = return((x + y):ys) 
foldingFunction (x:y:ys) "-" = return((y - x):ys) 
foldingFunction (x:y:ys) "/" = return((y / x):ys) 
foldingFunction (x:y:ys) "^" = return((y ** x):ys)  -- "5 2 ^"
foldingFunction (x:xs)  "ln" = return(log x:xs)     -- solveRPN "2.71828182846 ln 5 *"
foldingFunction xs  "sum"    = return([sum xs])     -- "1 2 3 sum"
-- foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)         -- here we are using the applicative-form "liftM == fmap ", 
foldingFunction xs numberString =  (readMaybe numberString) >>= \x-> return(x:xs) -- here we are using the monad application ">>="


                            
solveRPN :: String -> Maybe Double  
solveRPN st = do  
    [result] <- foldM foldingFunction [] (words st)  
    return result  

-- ghci>  foldM foldingFunction [] (words "15 4 3 + 2 * -")
-- Just [1.0]

-- ghci>  solveRPN "15 4 3 + 2 * -"
-- Just 1.0
