import Text.ParserCombinators.Parsec
import System.Environment
import System.Exit
import Data.Set

type PageOrdering = (Int, Int)
type ManualUpdate = [Int]

data SafetyManual = SafetyManual {
  orderingRules :: Set PageOrdering,
  manualUpdates :: [ManualUpdate]
} deriving (Eq, Show)

digits = many1 digit

parseUpdateFile :: GenParser Char st SafetyManual
parseUpdateFile = do
  orderingRules <- parseOrderingRules
  manualUpdates <- parseManualUpdates
  eof
  return (SafetyManual (fromList orderingRules) manualUpdates)

parseOrderingRules :: GenParser Char st [PageOrdering]
parseOrderingRules = do
  first <- parseOrderingRule
  rest <- remainingOrderingRules
  return (first : rest)

parseOrderingRule :: GenParser Char st PageOrdering
parseOrderingRule = do
  first <- digits 
  _ <- char '|'
  second <- digits
  return (read first, read second)

remainingOrderingRules :: GenParser Char st [PageOrdering]
remainingOrderingRules = try (newline >> newline >> (return [])) <|> try (newline >> parseOrderingRules) <|> (return [])

parseManualUpdates :: GenParser Char st [ManualUpdate]
parseManualUpdates = do
  first <- parseManualUpdate
  next <- remainingManualUpdates
  return (first : next)

remainingManualUpdates :: GenParser Char st [ManualUpdate]
remainingManualUpdates = (newline >> parseManualUpdates) <|> (return [])

parseManualUpdate :: GenParser Char st ManualUpdate
parseManualUpdate = do
  first <- digits
  next <- remainingManualUpdate
  return (read first : next)

remainingManualUpdate :: GenParser Char st ManualUpdate
remainingManualUpdate = (char ',' >> parseManualUpdate) <|> (return [])

applyParser :: String -> IO ()
applyParser input = case (parse parseUpdateFile "(unknown)" input) of
  Right safetyManual -> putStrLn (show safetyManual) >> exitSuccess
  Left error -> putStrLn ("ERROR - Unable to parse input: " ++ (show error)) >> exitFailure

parseArgs :: [FilePath] -> IO ()
parseArgs [] = putStrLn "ERROR - Expected a filename/path" >> exitFailure
parseArgs args = (readFile $ head args) >>= applyParser

main :: IO ()
main = getArgs >>= parseArgs
