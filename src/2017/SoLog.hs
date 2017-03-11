-- https://stackoverflow.com/questions/42702618/parsing-a-log-file-in-haskell/42714635#42714635

module SoLog where

-- import Data.List

type Name = String
type Count = Int
data MessageType =  Param Name Count
                 | Error String
                 | Unknown String
                   deriving (Show, Eq)

parseMessage :: String -> MessageType
parseMessage line =
    case  words line of
      ("parameter":n:_:c:_) -> Param n (read c)
      ("error":msg)         -> Error (unwords msg)
      xs                    -> Unknown $ unwords xs

data LogMessage = LogMessage Name Count [MessageType]
               deriving (Show, Eq)

parse :: String -> [MessageType]
parse = map parseMessage .  lines

isError :: MessageType -> Bool
isError (Error _) = True
isError _ = False


isUnknown :: MessageType -> Bool
isUnknown  (Unknown _)  = True
isUnknown _ = False

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||.) f g a = (f a) || (g a)

toLogMsg :: [MessageType] -> [LogMessage]
toLogMsg [] = []
toLogMsg (x:xs) =
    case x of
      Param n c ->
          LogMessage n c (takeWhile isError xs) : toLogMsg (dropWhile (isError .||. isUnknown) xs)
      _         -> toLogMsg $ dropWhile (isError .||. isUnknown) xs


errMsg :: [MessageType] -> [String]
errMsg = foldr (\(Error m) acc -> m : acc) []

toTriple :: [LogMessage] -> [(String, Count, [String])]
toTriple = foldl(\acc (LogMessage n c xs) -> (n, c, errMsg xs) : acc) []


main :: IO ()
main = do
       ts <- toLogMsg . parse <$> readFile "./src/2017/so-log.txt"
       mapM_ print (toTriple ts)
       mapM_ print ts
