import System.Environment
import System.Exit

-- Applies a function over a 3x3 sliding window 
thriceMap :: (a -> a -> a -> b) -> [a] -> [b]
thriceMap mapper (first:second:third:rest) = (mapper first second third) : (thriceMap mapper (second:third:rest))
thriceMap _ _ = []

thriceWindow :: [a] -> [(a, a, a)]
thriceWindow (a:b:c:xs) = (a, b, c) : thriceWindow (b:c:xs)
thriceWindow _ = []

scanXmas :: String -> String -> String -> Int
scanXmas ('M':c12:'S':upperRest) (_:'A':c23:middleRest) ('M':c32:'S':lowerRest) = 1 + (scanXmas (c12:'S':upperRest) ('A':c23:middleRest) (c32:'S':lowerRest))
scanXmas ('M':c12:'M':upperRest) (_:'A':c23:middleRest) ('S':c32:'S':lowerRest) = 1 + (scanXmas (c12:'M':upperRest) ('A':c23:middleRest) (c32:'S':lowerRest))
scanXmas ('S':c12:'S':upperRest) (_:'A':c23:middleRest) ('M':c32:'M':lowerRest) = 1 + (scanXmas (c12:'S':upperRest) ('A':c23:middleRest) (c32:'M':lowerRest))
scanXmas ('S':c12:'M':upperRest) (_:'A':c23:middleRest) ('S':c32:'M':lowerRest) = 1 + (scanXmas (c12:'M':upperRest) ('A':c23:middleRest) (c32:'M':lowerRest))
scanXmas (_:xs) (_:ys) (_:zs) = scanXmas xs ys zs
scanXmas [] _ _ = 0
scanXmas _ [] _ = 0
scanXmas _ _ [] = 0

xmasCounter :: String -> Int
xmasCounter = foldl (+) 0 . thriceMap scanXmas . lines

parseArgs [] = putStrLn "ERROR - Expected a filename/path" >> exitWith (ExitFailure 1)
parseArgs args = (readFile $ head args) >>= putStrLn . show . xmasCounter >> exitWith ExitSuccess

main :: IO ()
main = getArgs >>= parseArgs  
  -- fileContents <- readFile "../inputs/input_day4"
  -- let result = xmasCounter fileContents
  -- putStrLn (show result)

