import Data.List (nub, sort)  
import qualified Data.Map as Map
import C05C06

numUniques :: (Eq a) => [a] -> Int  
--nub is a function defined in Data.List that takes a list and weeds out duplicate elements. 
numUniques = length . nub  


--if a key isn't in the "association list"/Map, we'll end up trying to get the head of an empty list, which throws a runtime error. 
findValue key xs = snd . head . filter (\(k,v) -> key == k) $ xs  

fnn = collatzSeqGT15

-- If we don't find the key, we'll return a Nothing. If we find it, we'll return Just something, where something is the value corresponding to that key.
findValue_ :: (Eq k) => k -> [(k,v)] -> Maybe v  
findValue_ key [] = Nothing  
findValue_ key ((k,v):xs) = if key == k  
                            then Just v  
                            else findValue_ key xs  


-- implemented as a fold
findValue_' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing  


phoneBook =   
    [("1","aaaa")  
    ,("1","bbbb")  
    ,("2","cccc")  
    ,("3","dddd")  
    ,("3","eeee")  
    ,("3","ffff")  
    ,("4","gggg")  
    ,("4","hhhh")  
    ,("5","iiii")  
    ,("5","jjjj")  
    ]  


-- If a duplicate key is found, the function we pass is used first make all the values in the association list singleton lists and then we can use ++ to combine the numbers.
phoneBookToMap  = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) phoneBook  
-- ghci>   phoneBookToMap 
-- fromList [("1",["bbbb","aaaa"]),("2",["cccc"]),("3",["ffff","eeee","dddd"]),("4",["hhhh","gggg"]),("5",["jjjj","iiii"])]
-- ghci>  Map.lookup "1" $ phoneBookToMap 
-- Just ["bbbb","aaaa"]


data LockerState = Taken | Free deriving (Show, Eq)  

lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of     -- notice the "case lookup key map of"
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"  
