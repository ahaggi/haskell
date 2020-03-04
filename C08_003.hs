module C08_003(
    Tree(..),
    singleton,
    treeInsert,
    treeElem
)where

import qualified Data.Map as Map

data Vector a = Vector a a a deriving (Show)  


type V1 a = Vector a
type V2 a = Vector a

vplus :: (Num t) => V1 t -> V2 t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  



vectMult :: (Num t) => V1 t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
-- ghci>  vectMult (Vector 1 2 3) 3
-- Vector 3 6 9



scalarMult :: (Num t) => V1 t -> V2 t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  
-- ghci>  (Vector 1 2 3) `vectMult` 3
-- Vector 3 6 9




-- To extract a value from Maybe we can do the flwg:
    -- use the prelude function called "maybe" 
        -- Prelude> maybe "" id (Just "value")
        -- "value"

    -- Or
-- extractaValueFromMaybe ::Maybe a ->a->a
-- extractaValueFromMaybe param inCaseOfNothing  = case param of
--                                                         Nothing -> inCaseOfNothing
--                                                         (Just v) ->  v
    




data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday    deriving (Eq, Ord, Show, Read, Bounded, Enum)  

-- ************************************************************************************************************************** --
-- ************************************************************************************************************************** --
--  This is roughly how Either is defined   data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)  
data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  
  
type LockerMap = Map.Map Int (LockerState, String)  

lockerLookup :: Int -> Either String String  
lockerLookup lockerNumber  =   
    case Map.lookup lockerNumber lockers of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"  

                                
-- lookup function search the map with some key, and return either "Nothing" or "Maybe v"
-- case .. of return LockerState, either Left or Right 



lockers = Map.fromList   
        [(100,(Taken,"AAA"))
        ,(101,(Free,"BBB"))
        ,(103,(Free,"CCC"))
        ,(105,(Free,"DDD"))
        ,(109,(Taken,"EEE"))
        ,(110,(Taken,"FFF"))]

-- We could have used a Maybe a to represent the result but then we wouldn't know why we couldn't get the code. 
-- But now, we have information about the failure in our result type.

infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)  

infixr 5  .++  
(.++) :: List a -> List a -> List a   
Empty .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys)  

-- ghci>  let xs = 3 :-: 4 :-: 5 :-: 6 :-: Empty
-- ghci>  let ys = 1 :-: 2 :-: Empty
-- ghci>  xs .++ ys
-- 3 :-: (4 :-: (5 :-: (6 :-: (1 :-: (2 :-: Empty)))))


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)  

-- ghci>    treeInsert 8 $ treeInsert 6 $ treeInsert 1 $ treeInsert 4 $ treeInsert 3 treeInsert 7  (treeInsert 5 EmptyTree)
-- ghci> let numsTree = foldr treeInsert EmptyTree [8,6,4,1,7,3,5]  
-- Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

-- is x a node in the tree  EmptyTree [8,6,4,1,7,3,5]  
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
                        | x == a = True
                        | x < a = treeElem x left
                        | x > a = treeElem x right

-- ghci>  let root = foldr treeInsert EmptyTree [8,6,4,1,7,3,5]

-- ghci>  treeElem 4 root
-- True
-- ghci>  treeElem 2 root
-- False
-- ghci>  treeElem 8 root
-- True



data Chunk c =  EmptyChunk | Chunk c  (Chunk c ) deriving (Show, Read, Eq)

-- ghci>   (Chunk "123" ( Chunk"456" EmptyChunk))
-- Chunk "123" (Chunk "456" EmptyChunk)
