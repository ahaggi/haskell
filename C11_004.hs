import C08_003
import qualified Data.Foldable as F  
 
-- class Monoid m where
--     mempty :: m
--     mappend :: m -> m -> m
--     mconcat :: [m] -> m
--     mconcat = foldr mappend mempty


-- instance Monoid [a] where
--     mempty = []
--     mappend = (<>)
--     --mconcat = foldr mappend mempty  -- we don't need to implement this, it's already defined in Monoid typeClass




newtype Product a =  Product { getProduct :: a }  deriving (Eq, Ord, Read, Show, Bounded)
newtype Sum a =  Sum { getSum :: a }  deriving (Eq, Ord, Read, Show, Bounded)

-- OLD implementation
-- instance (Num a) => Monoid (Product a) where
--     mempty = Product 1
--     Product x `mappend` Product y = Product (x * y)

-- since /base-4.11.0.0/   March 2018
instance (Num a) => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)
instance (Num a) => Monoid (Product a) where
    mempty = Product 1
    -- mappend = (<>) -- we don't need to implement this, it's already defined



 
-- instance (Num a) => Monoid (Sum a) where
--     mempty = Sum 0
--     Sum x `mappend` Sum y = Sum (x + y)

-- since /base-4.11.0.0/   March 2018
instance (Num a) => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)
instance (Num a) => Monoid (Sum a) where
    mempty = Sum 0
    -- mappend = (<>)-- we don't need to implement this, it's already defined







-------------------------------------------------------------------------------

-- lengthCompare x y = let a = length x `compare` length y
--                         c = x `compare` y
--                         vowels = length . filter (`elem` "aeiou")
--                         b = vowels x `compare` vowels y
--                     in  if a == EQ
--                             then (if b == EQ
--                                     then c
--                                     else b)
--                             else a


lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
            where vowels = length . filter (`elem` "aeiou")




-- data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  



-- implementing foldMap is all it takes for our type to be made an instance of Foldable. 
-- So if we just implement foldMap for some type, we get foldr and foldl on that type for free! 
instance F.Foldable Tree where  
    foldMap f EmptyTree = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r  



