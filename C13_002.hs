newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  

fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []  



instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))  

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)  
    mappend = (<>)  


xs = [1..1000]
xs_ = toDiffList[1..1000]
ff' ys 0 = ys
ff' ys n = (ff' ys (n-1)) `mappend` ys

-- ghci> fromDiffList (ff' xs 100)
-- ghci> ff' xs_  100

calcSomething  = do
                    x <- (*3)  
                    y <- (+6)  
                    z <- (*2)  
                    return ((x + y) / z )   