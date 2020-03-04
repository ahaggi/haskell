
doubelx :: Num a => a -> a
-- doubelx ::Int -> Int  -- OK if we want to use "doubelx" just for int 
-- doubelx :: Num -> Num -- Err  Expected a type, but ‘Num’ has kind ‘* -> Constraint’, which means the GHCI expected a "Type" but "Num" is "Typeclass"

doubelx x  = x + x

doubelxy :: Num a => a -> a -> a
-- doubelxy :: Int -> Int -> Int -- OK if "doubelx" have the same type and  we want to use "doubelxy" just for int
doubelxy x y = (doubelx x) + ( doubelx y)

prefixVSinfix :: Integral a => a -> Bool
prefixVSinfix x = div x 2  == x `div` 2

-- takes a list of some type and return some value of type a,, as long as "a" implements the typeclass "Num" 
-- => symbol is called a class constraint, the constraint is (as long as "a" impl ..)
length' :: Num a => [t] -> a
length' xs = sum [1 | _ <- xs]

listReplaceElmWithOnes:: Num a => [t] -> [a]
listReplaceElmWithOnes xs = [1 | _ <- xs]

generateListDart :: (Num t, Enum t) => t -> (t -> a) -> [a]
-- generateListDart :: (Integral t) => t -> (t -> a) -> [a]
generateListDart n fn = [ fn x | x <- [1..n]]

regxOnlyFirstLetterUpperCase :: [[Char]] -> [[Char]]
regxOnlyFirstLetterUpperCase xs = [ x | x <- xs , head x `elem` ['A'..'Z'] ] -- x is String witch is a list of chars

onlyEven :: Integral a => [a] -> [a]
onlyEven xs = [ x | x <- xs, even x ]

onlyEvenXXs :: Integral a => [[a]] -> [[a]]
onlyEvenXXs xxs =[ onlyEven xs | xs <- xxs]

onlyEvenXXs1 :: Integral a => [[a]] -> [[a]]
onlyEvenXXs1 xxs = [ [ x | x <- xs, even x ] | xs <- xxs] -- remove the odd numbers of every list in xxs,, where xxs is list of num lists

-- takes param of type t and return value of type t,, as long as t implements the type classes Eq and Num
-- => symbol is called a class constraint, the constraints are (as long as t impl ..)
-- Eq typeclass provides an interface for testing for equality. Any type where it makes sense to test for equality between two values of that type should be a member of the Eq class
factorial :: (Eq t, Num t) => t -> t
-- factorial :: (Integral a) => a -> a  Because members of Integral are also members of Eq and Num
factorial x = if x == 1 then x else x * factorial (x-1)


infixl 5 <:  
(<:) :: Num a => a -> a -> a
x <: y = (x*y) + (x*y)
