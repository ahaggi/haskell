import  Control.Monad  
import Data.List (nub)  


applyMaybe Nothing f  = Nothing
applyMaybe (Just x) f = f x


-------------------------------------------------
--These 2 function takes a number of birds and lands them on one side of the pole. 
landLeft :: Int -> (Int,Int) -> (Int,Int)  
landLeft n (lf,rt) = (lf + n,rt)  


landRight :: Int -> (Int,Int) -> (Int,Int)  
landRight n (lf,rt) = (lf,rt + n)  


-- ghci>  landLeft 2 . landRight 1 . landLeft 1 $(0,0)
-- (3,1)


-- the flwg function will take a param first and then a function, and apply that function to the param
x -: f = f x  

-- ghci>  (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2 
-- (3,1)


-- ******************************************************************************************************
-- * Problem1: we need to return the value "Nothing", if the diff between the both sides is more than 4 *
-- ******************************************************************************************************

-- and if diff between the both sides is Eq or more than 4 return Nothing
landLeft' :: Int -> (Int,Int) -> Maybe (Int,Int)  
landLeft' n (lf,rt)  
    | abs ((lf + n) - rt) < 4 = Just (lf + n, rt)  
    | otherwise                    = Nothing  

landRight' :: Int -> (Int,Int) -> Maybe (Int,Int)  
landRight' n (lf,rt)  
    | abs (lf - (rt + n)) < 4 = Just (lf, rt + n)  
    | otherwise                    = Nothing  

-- ******************************************************************************************************
-- * Problem2: But now we can't compise landLeft/landRight, becuase both take "Int" return a "Maybe"    *
-- ******************************************************************************************************
-- landLeft 2 . landRight 1 . landLeft 1 $(0,0)      OR      (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2 
-- landLeft 1 (0,0) OK!
-- landRight 1 Maybe(Int,Int) Err!
-- We need a way of taking a Maybe(Int,Int) and feeding it to a function that takes a (Int,Int)



-- ghci>  landLeft 1 (0,0) >>= landRight 1 >>= landLeft 2 

-- ghci>  (0,0) -: landLeft 1 >>= landRight 1 >>= landLeft 2

-- ghci>  return (0,0) >>= landLeft 1 >>= landRight 1 >>= landLeft 2

-- Just (3,1)



-- Without chain of monadic applications with >>=,  here's how a series of bird landings would look like:
routine = case landLeft' 1 (0,0) of  
    Nothing -> Nothing  
    Just pole1 -> case landRight' 1 pole1 of   
        Nothing -> Nothing  
        Just pole2 -> landLeft' 2 pole2 


-- using do notation

routine' = do  
    start <- return (0,0)  
    first <- landLeft' 2 start 
    -- _  <-  Nothing   
    second <- landRight' 1 first  
    landLeft' 2 second  


-- ******************************************************************************************************
-- * Problem3: But returning nothing does not explain what went wrong. Use Either l r    *
-- ******************************************************************************************************



landLeft'' :: Int -> (Int,Int) -> Either String (Int,Int)  
landLeft'' n (lf,rt)  
    | abs ((lf + n) - rt) < 4 = Right (lf + n, rt)  
    | otherwise                    = Left ("fell becuase there was " ++ show (lf+n) ++ " birds on the left side, and " ++ show rt ++ " on the right side.")  

landRight'' :: Int -> (Int,Int) -> Either String (Int,Int)  
landRight'' n (lf,rt)  
    | abs (lf - (rt + n)) < 4 = Right (lf, rt + n)  
    | otherwise                    =  Left ("fell becuase there was " ++ show lf ++ " birds on the left side, and " ++ show (rt+n) ++ " on the right side.")    


routine'' = do  
    start <- return (0,0)  
    first <- landLeft'' 2 start 
    second <- landRight'' 1 first  
    landLeft'' 2 second  


    -- ********************************************************************************************************************


type Col = Int
type Row = Int
moveKnight  :: (Col,Row) -> [(Col,Row)]  
moveKnight  (c,r) = do  
    -- 8 possible moves is the roof limit, but some cells will have only 6,4, or 2 moves,, take a look at the file C12_moveKnight.png
    (c',r') <- [ (c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
                ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  -- for each tuple of (c',r'), check if the tuple have avalid cell values, if not replace it with []
    return (c',r')  


allPossiblePathsInThree fromCell@(c,r) toCell@(c',r')= return fromCell >>= 
                                                                    \strt -> moveKnight strt >>= 
                                                                    \fst-> moveKnight fst >>= 
                                                                    \snd-> moveKnight snd >>= 
                                                                    \thrd-> guard(thrd == toCell ) >> [[strt , fst , snd , thrd]]
                                                              -- we could use: \third->.... >> return ([strt , fst , snd , thrd])


-- allPossiblePathsInThree fromCell@(c,r) toCell@(c',r')= return fromCell >>= 
--     \strt -> moveKnight strt >>= 
--     \fst-> moveKnight fst >>= 
--     \snd-> moveKnight snd >>= 
--     \thrd-> if(thrd == toCell ) then 
--             [[strt , fst , snd , thrd]]
--             else []

allPossiblePathsInThree' fromCell@(c,r) toCell@(c',r')= do
                                                strt <- [fromCell]  -- NOTICE "return fromCell" / [fromCell] the right side of "<-" must be monadic
                                                fst  <- moveKnight strt
                                                snd  <- moveKnight fst 
                                                thrd <- moveKnight snd
                                                guard (thrd == toCell) 
                                                [[fromCell, fst , snd , thrd]]
                        -- we could use: return ([fromCell, fst , snd , thrd])

                        
allPossibleMovesInThree fromCell@(c,r)= return fromCell >>= moveKnight >>= moveKnight >>= moveKnight


-- allPossibleMovesInThree start = nub $  do   
--                         first <- moveKnight start  
--                         second <- moveKnight first  
--                         moveKnight second  


canBeReachedInThreeMoves fromCell@(c,r)  toCell@(c',r') = toCell `elem` (allPossibleMovesInThree fromCell)



    -- **************************************************************************************************************************************
    --                                            using composition for monadic functions
    -- **************************************************************************************************************************************



--       foldr (<=<) (return) [ moveKnight , moveKnight , moveKnight ... ] will compose all the "n" copies of the function moveKnight into one
--       Read more about "Composing monadic functions"   at  "C13 Monads More6 - liftM ap liftA2 liftM3".
allPossibleMovesInMany n fromCell@(c,r) = return fromCell >>= foldr (<=<) return (replicate n moveKnight)  

canBeReachedInManyMoves n fromCell@(c,r) toCell@(c',r') = toCell `elem` (allPossibleMovesInMany n fromCell)

allPossiblePathsInMany n fromCell@(c,r) = return fromCell >>= foldr (<=<) (return (replicate n moveKnight) )




-- nn n _ 
--         | n<= 0 = [] 
-- nn n [] =  []
-- nn n (x: xs) =   (moveKnight x) ++ [ (0,0) ] ++(nn (n-1) (moveKnight x) ) ++ (nn (n) (xs) )



