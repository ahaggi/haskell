import Control.Monad.State
import System.Random

type Stack = [Int]


pop_ :: Stack -> (Int, Stack)
pop_ (x:xs) = (x, xs)


push_ :: Int -> Stack -> ((), Stack)
push_ x xs = ((), x : xs)


stackManip_ :: Stack -> (Int, Stack)
stackManip_ stack =
  let ((), newStack1) = push_ 3 stack
      (a, newStack2) = pop_ newStack1
   in pop_ newStack2


-- newtype State s a = state { runState :: s -> (a,s) }
pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)
-- pop = state $ pop_

-- ghci>  (runState pop) [2,3,4]
-- [3,4]
push :: Int -> State Stack ()
push a = state $ \xs -> ((), a : xs)
-- push a = state $ push_

-- ghci>  (runState (push 3)) [2,3,4]
-- ghci>  ( \xs -> ((),3:xs) ) [2,3,4]
-- ((),[3,2,3,4])
stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop

calc' x = calc x

-- state Num String
calc :: (Fractional a, Show a) => a -> String -> a -> State String a
calc prevValue f n =
  let newValue = (fn f) prevValue n
        where
          fn "+" = (+)
          fn "-" = (-)
          fn "*" = (*)
          fn "/" = (/)
   in state (\s -> (newValue, (show n) ++ f ++ s))

-- *****************************************************************************************************************************************
-- ghci>  (runState ( pop >>= \prevState1-> pop >>= \prevState2-> push 0  >>= \prevState3-> (return 'X') :: State Stack Char   )) [1,2]
--                 (x:y:xs)    (y:xs)                (xs)                       (0:xs)
-- ('X',[0])


--Let us say p = pop,
--           k = \resP-> pop

(||=) :: State s a -> (a -> State s b) -> State s b
p ||= k = q
  where
    p' = runState p   -- p' :: s -> (a, s)
    k' = runState . k -- k' :: a -> s -> (b, s)  NOTICE the composition becuase k is  "\prevValue-> pop" NOT "pop"
    q =
      state $ \state0 ->
                    let (resP, state1) = p' state0
                    in (k' resP) state1


p |>= k =
  state $ \s0 ->
     let (resP, s1) = (runState p) s0 -- Running the first processor on s0.
     in (runState (k resP)) s1        -- Running the second processor on s1.


(|+|) :: State s a -> (a -> State s b) -> State s b
h |+| f =
  state $ \s ->
    let (resP, newState) = (runState h) s
        g = runState (f resP)
    in  g newState


-- *****************************************************************************************************************************************
-- *****************************************************************************************************************************************


pop3times =
  get >>= \initState ->
    (if (length initState >= 3) --NOTICE that the res of both (then & else) in the if expression has to be the same type, i.e. monadic value of type State /statefulComputation.
       then return ()
       else put [1, 2, 3, 4]) >>= \prevRes -> pop >>= \prevRes -> pop >>= \prevRes -> pop

-- ghci>  runState pop3times [7,8,9]
-- (9,[])
-- ghci>  runState pop3times [7,8]
-- (3,[4])



threeCoins' :: StdGen -> (Bool, Bool, Bool)
threeCoins' gen =
            let (firstCoin, newGen) = random gen
                (secondCoin, newGen') = random newGen
                (thirdCoin, newGen'') = random newGen'
            in (firstCoin, secondCoin, thirdCoin)
-- ghci>  threeCoins' (mkStdGen 100)
-- (True,False,False)
            
randomSt :: (RandomGen g, Random a) => State g a  
randomSt = state random  

threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    firstCoin  <- randomSt  
    secondCoin <- randomSt  
    thirdCoin  <- randomSt  
    return (firstCoin, secondCoin, thirdCoin)  

-- ghci>  evalState threeCoins (mkStdGen 100)
-- (True,False,False)


-- *****************************************************************************************************************
-- *********************************https://wiki.haskell.org/State_Monad********************************************
-- *****************************************************************************************************************

-- TRY TO REIMPLEMENT THIS EXAMPLES

-- Example use of State monad
-- Passes a string of dictionary {a,b,c}
-- Game is to produce a number from the string.
-- By default the game is off, a C toggles the
-- game on and off. A 'a' gives +1 and a b gives -1.
-- E.g 
-- 'ab'    = 0
-- 'ca'    = 1
-- 'cabca' = 0
-- State = (game is on or off ,  current score)
--       = (Bool, Int)

type GameValue = Int
type GameState = (Bool, Int)

startState = (False, 0::Int)

playGame :: String -> State GameState GameValue
playGame []     = do
    (_, score) <- get
    return score

playGame (x:xs) = do
  (isOn, score) <- get
  case x of
        'a' |isOn ==True -> put (isOn, score + 1)         -- Notice that the statefulComputation is ( ()  , (Bool, Int) )
        'b' |isOn -> put (isOn, score - 1)                --                                          res ,    state  ,,, the "res" never used to compute the next state
        'c'      -> put (not isOn, score)
         _       -> put (isOn, score)    -- IF x is not a, b or c
  playGame xs


-- Notice that we didn't use otherwise here. Becuase "isOn" can be True/False,
--   'a' | isOn -> put (isOn, score + 1)  
-- and could be rewriten as
-- 'a' 
--     | isOn ==True -> put (isOn, score + 1)       
--     | otherwise -> put (isOn, score )     


-- playGame [] = get >>= \(_, score)-> return score
-- playGame (x:xs) = do
--     (isOn, score) <- get
--     case (x, isOn) of          -- Notice how we created a tupple of (anInput , resOfthePrevComputation)
--          ('a' , True) -> put (isOn, score + 1)
--          ('b' , True) -> put (isOn, score - 1)
--          ('c' , _)    -> put (not isOn, score)
--          _        -> put (isOn, score)   -- when 'a' or 'b' and the prevState is false,, And IF x is not a, b or c
--     playGame xs


-- ghci>  evalState (playGame "abcaaacbbcabbab") startState
-- 2
-- ghci>  runState (playGame "abcaaacbbcabbab") startState
-- (2,(True,2))

-- *****************************************************************************************************************

-- parse x:xs
-- if the last list in the state is the same type as x 
    -- append x to the last list in the state
    -- otherwise create a new list and append x to it, after that append that list to the state
-- Do not use the prevRes in the current statefulComputation

type ParsingState = (Char , [ String ])
type ParsingRes = ()

initState = ('!' , [])

parse:: String -> State ParsingState ParsingRes
parse [] = get >>= \((ch, listOfstr))-> put ('$' , reverse  . fmap  reverse $listOfstr)

parse (x:xs) =
              do
                (ch , listOfstr )<-get
                case listOfstr of
                  [] -> state $ \prevState-> ((), (x, [x]:[]))
                  (ys:yss) 
                    | (bothSameType x ch) -> put (x, (x:ys):yss )
                    | (bothDiffType x ch) -> put (x, [x]:ys:yss )                        --    ==  (isNum x && isAlphabet ch) || (isAlphabet x && isNum ch)
                    | otherwise -> put (ch , listOfstr) -- ignor any ([^a-zA-Z0-9]) char 
                    where bothSameType x' ch'= (isNum x' && isNum ch') || (isAlphabet x' && isAlphabet ch')
                          bothDiffType x' ch'= (isNum x' && isAlphabet ch') || (isAlphabet x' && isNum ch')
                          isNum n      = n `elem` ['0'..'9']
                          isAlphabet c = c `elem` ['a'..'z']++['A'..'Z']
                parse xs
  -- ghci>  snd $execState (parse "abNc123Qm553g") initState
  -- ["abNc","123","Qm","553","g"]


-- The same implementation but without using get and put
-- parse (x:xs) =
--   do
--     (ch , listOfstr )<- state $ \s -> (s,s)  -- where s is (ch , listOfstr)
--     case listOfstr of
--       [] -> state $ \prevState-> ((), (x, [x]:[]))
--       (ys:yss) 
--         | ((x `elem` ['0'..'9'] && ch `elem` ['0'..'9']) ||(x `elem` ['a'..'z'] && ch `elem` ['a'..'z'])) ->  state $ \prevState-> ((), (x, (x:ys):yss) )
--         | ((x `elem` ['0'..'9'] && ch `elem` ['a'..'z']) ||(x `elem` ['a'..'z'] && ch `elem` ['0'..'9'])) ->  state $ \prevState-> ((), (x, [x]:ys:yss ) )
--         | otherwise -> put (ch , listOfstr) -- ignor any ([^a-zA-Z0-9]) char 
--     parse xs



