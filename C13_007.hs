import Data.Ratio  
import  Control.Monad  


newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show  


instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs  
-- ghci>  fmap (*2)  $ Prob [(3,1%2),(5,1%4),(9,1%4)]
-- Prob {getProb = [(6,1 % 2),(10,1 % 4),(18,1 % 4)]}




-- 1.st approach to create a monad is : m >>= f = flatten' (fmap (getProb.ff) m)  

flatten' :: Prob [(a, Rational)] -> Prob a
flatten' (Prob xs) = Prob $ concat $ map multAll xs  
            where multAll (innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs  

-- ghci>   (fmap (getProb.ff) m)
-- Prob {getProb = [
--                 ([(3,1 % 2),(-3,1 % 2)],1 % 2),
--                 ([(5,1 % 2),(-5,1 % 2)],1 % 4),
--                 ([(9,1 % 2),(-9,1 % 2)],1 % 4)
--                 ]}


-- 2.st approach to create a monad is : m >>= f = flatten (fmap ff m)  
flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs  
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs  

-- ghci>  fmap ff m
-- Prob {getProb = [
--                 (Prob {getProb = [(3,1 % 2),(-3,1 % 2)]},1 % 2),
--                 (Prob {getProb = [(5,1 % 2),(-5,1 % 2)]},1 % 4),
--                 (Prob {getProb = [(9,1 % 2),(-9,1 % 2)]},1 % 4)
--                 ]}



instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)
    fail _ = Prob []  


instance Applicative Prob where
  pure  = return
  (<*>) = ap

-- *********************************************************************************************************
-- *********************************************************************************************************

data Coin = Heads | Tails deriving (Show, Eq)
    
coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]
    
loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]



flipThree = do  
    a <- coin  
    b <- coin  
    c <- loadedCoin  
    -- return (all (==Tails) [a,b,c])  
    return  b