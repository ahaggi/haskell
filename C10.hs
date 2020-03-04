-- function that takes as its parameter a string that contains a RPN expression, like "10 4 3 + 2 * -" and gives us back its result


cl = foldl foldingFunction []
    where foldingFunction 




solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  -- notice we process just the first number in the acc
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs  



-- *******************************************************************************************************************************************
-- *******************************************************************************************************************************************
-- *******************************************************************************************************************************************



heathrowToLondon' = [(50,10,30),(5,90,20),(40,2,25),(10,8,0)]


shortestsPath :: (Integral a) => [(a,a,a)]-> [(a,a)]
shortestsPath = reverse . foldl foldingFunction [(0,0)] 
    where foldingFunction ((a,b):ns) (e1,e2,e3) = ( min (a + e1) (b + (e2+e3))  , min (b +e2) (a + (e1+e3)) ): (a,b) : ns 

-- shortestsPath' :: (Show a , Integral a) => [(a,a,a)]-> [(a,String,a,String)]
-- shortestsPath' = reverse . foldl foldingFunction [(0, "" ,0, "")] 
--     where foldingFunction ((a, aPath , b , bPath):ns) (e1,e2,e3) = (  min (a + e1) (b + (e2+e3))  ,
--                                                                     if (a + e1) < (b + (e2+e3)) 
--                                                                         then  aPath ++"->" ++ show e1
--                                                                         else  bPath ++"->" ++ show e2 ++ "->"++ show e3
--                                                                         ,
--                                                                     min (b +e2) (a + (e1+e3)) ,
--                                                                     if (b +e2) < (a + (e1+e3))
--                                                                         then    bPath ++"->" ++ show e2
--                                                                         else    aPath ++"->" ++ show e1 ++ "->"++ show e3 ): (a, aPath , b , bPath) : ns 

shortestsPath' :: (Show a , Integral a) => [(a,a,a)]-> (a,String,a,String)
shortestsPath' =  path 

-- shortestsPath' :: (Show a , Integral a) =>  [(a,a,a)]->(a,String)
-- shortestsPath' xs=  let (aCost,ap,bCost,bp) = path xs
--                   in if (aCost < bCost)  then (aCost , ap) else (bCost, bp)

    where   path = last . listOfShorstestPath -- notice the pointless pattern,, we didn't write "shortestsPath' xs" but "shortestsPath' "
            listOfShorstestPath = reverse . foldl foldingFunction [(0, "" ,0, "")]
            -- foldingFunction ((a, aPath , b , bPath):ns) (e1,e2,e3) = ( fst calcAnodsRes, snd calcAnodsRes , fst calcBnodsRes , snd calcBnodsRes ) : (a, aPath , b , bPath) : ns 
            foldingFunction    ((a, aPath , b , bPath):ns) (e1,e2,e3) = (,,,) (fst calcAnodsRes) (snd calcAnodsRes)  (fst calcBnodsRes)  (snd calcBnodsRes)  : (a, aPath , b , bPath) : ns 
                    where   calcAnodsRes = calcAnods ( a, aPath , b , bPath , e1 , e2 , e3 )
                            calcBnodsRes = calcBnods ( a, aPath , b , bPath , e1 , e2 , e3 )
                            calcAnods ( a, aPath , b , bPath , e1 , e2 , e3 ) = if (a + e1) < (b + (e2 + e3)) then ((a + e1) , aPath ++"->" ++ show e1) else ((b + (e2+e3)) , bPath ++"->" ++ show e2 ++ "->"++ show e3)
                            calcBnods ( a, aPath , b , bPath , e1 , e2 , e3 ) = if (b + e2) < (a + (e1 + e3)) then ((b + e2) , bPath ++"->" ++ show e2) else ((a + (e1+e3)) , aPath ++"->" ++ show e1 ++ "->"++ show e3)

-- The (,,) function is the same as \x y z -> (x,y,z). Also, the (,) function is the same as \x y -> (x,y). So (,,,) 1 2 3 4 ==> (1,2,3,4)

-- ghci>  shortestsPath' heathrowToLondon'
-- (75,"->10->30->5->20->2->8->0",75,"->10->30->5->20->2->8")
                            



-- *******************************************************************************************************************************************




-- A node is, and has information about either 
    -- a normal node has 2 road:  the road that leads to the other main road and the road that leads to the next node, or,
    -- an end node has just 1 road: the road (with cost of zero) that leads to the other main road 
data Node = Node Road Road | EndNode Road  
data Road = Road Int Node  

-- 50, 10, 30, 5, 90, 20, 40, 2, 25, and 10, 8, 0. can be represented as sections
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
-- RoadSystem is a list of sections
type RoadSystem = [Section]  

heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]  


data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)]  


-- roadStep a function which takes just one section (which leads to next paths) and the previous paths which led to that section
-- For the first section (Section 50 10 30)  the previous paths are ( []               ,  []       )   ==> ( [(C,30),(B,10)]        ,  [(B,10)]                     )
-- For the first section (Section 5  90 20)  the previous paths are ( [(C,30),(B,10)]  ,  [(B,10)] )   ==> ( [(A,5),(C,30),(B,10)]  ,  [(C,20),(A,5),(C,30),(B,10)] )

--         Notice that the result paths are prepended, so read them from right to left

roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A,a):pathA  
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)  


-- ghci>  optimalPath heathrowToLondon
-- [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]
optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
            then reverse bestAPath  
            else reverse bestBPath  



-- *******************************************************************************************************************************************
-- *******************************************************************************************************************************************
-- *******************************************************************************************************************************************



