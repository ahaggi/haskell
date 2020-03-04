import           Control.Monad
import           Data.Char
import           System.Environment
import           System.Random

main = askForNumberMain

-- ***********************************************************************************************************************************
printInputUntilWhitespace = do
  c <- getChar
  if c /= ' '
    then do
      putChar c
      main
    else return ()

printInputUntilWhitespace' = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    main

-- ***********************************************************************************************************************************
-- Nice sulotion to dispatch "main" function's call to other functions
-- Take a look at the implementation in the book "C09 Input and Output - Command line arguments"
--      ./C09_001 "printInput2'"
--      OR
--      cat './C09 1Input and Output' | ./C09_001 "printInput2'"
dispatch :: [(String, IO ())]
dispatch =
  [ ("printInput1", printInput1)
  , ("printInput2", printInput2)
  , ("printInput2'", printInput2')
  ]

frontDispatcher = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
    -- action args
  action

printInput1 :: IO ()
printInput1 = do
  putStrLn "Enter a string:"
  str <- getLine
  putStrLn ("The input is \"" ++ str ++ "\"")

-- cat someTextFile | ./C09 ,, we can read the content of someTextFile and print it content, with some help of using bash piping.
printInput2 :: IO ()
printInput2 =
  forever $ do
    l <- getLine
    putStrLn $ map toUpper l

printInput2' :: IO ()
printInput2' = do
  contents <- getContents
  putStr (map toUpper contents)

-- ***********************************************************************************************************************************
reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- ***********************************************************************************************************************************
-- ***********************************************************************************************************************************
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
   in (firstCoin, secondCoin, thirdCoin)

-- ***********************************************************************************************************************************
askForNumberMain = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  _ <- putStr "Which number in the range from 1 to 10 am I thinking of? "
  numberString <- getLine
  when (not $ null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    askForNumber newGen
