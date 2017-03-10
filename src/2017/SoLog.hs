-- https://stackoverflow.com/questions/42702618/parsing-a-log-file-in-haskell/42714635#42714635

module SoLog where

import Data.List

type Name = String
type Count = Int
data LogMessage = Param Name Count
                | Error String
                | Unknown String
                  deriving (Show, Eq)

parseMessage :: String -> LogMessage
parseMessage line =
    case  words line of
      ("parameter":n:_:c:_) -> Param n (read c)
      ("error":msg)         -> Error (unwords msg)
      xs                    -> Unknown $ unwords xs

data LogTriple = LogTriple Name Count [LogMessage]
               deriving (Show, Eq)

parse :: String -> [LogMessage]
parse = map parseMessage .  lines

isError :: LogMessage -> Bool
isError (Error _) = True
isError _ = False


isUnknown :: LogMessage -> Bool
isUnknown  (Unknown _)  = True
isUnknown _ = False
(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||.) f g a = (f a) || (g a)
toTriples :: [LogMessage] -> [LogTriple]
toTriples [] = []
toTriples (x:xs) =
    case x of
      Param n c ->
          LogTriple n c (takeWhile isError xs) : toTriples (dropWhile (isError .||. isUnknown) xs)
      _             -> toTriples $ dropWhile (isError .||. isUnknown) xs


main :: IO ()
main = do
       ts <- toTriples . parse <$> readFile "./so-log.txt"
       mapM_ print ts
