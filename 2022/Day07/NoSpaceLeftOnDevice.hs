module Day07.NoSpaceLeftOnDevice where
import Control.Monad.State
    ( evalState, MonadState(put, get), State )
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Data.List (isPrefixOf)

-- TODO: Clean this up

main :: IO ()
main = do
    xs <- lines <$> readFile "2022/data/day07.txt"
    let [root] = evalState prompt xs
        dirSizes = map (\(Directory _ sz _) -> sz) . filter isDir . toList . totalSize $ root
    let part1 = sum . filter (<= 100000) $ dirSizes
    let spaceNeeded = 30000000 - (70000000 - head dirSizes)
    let part2 = minimum . filter (>= spaceNeeded) $ dirSizes
    print (part1, part2)

data Entry = File String Int | Directory String Int [Entry] deriving (Eq, Ord, Show)

isDir :: Entry -> Bool
isDir (Directory {}) = True
isDir _ = False

totalSize :: Entry -> Entry
totalSize f@(File _ _) = f
totalSize (Directory name _ entries) =
    let entries' = map totalSize entries
        size = sum $ map getSize entries'
    in Directory name size entries'
  where
    getSize (File _ sz) = sz
    getSize (Directory _ sz entries') = sz

toList :: Entry -> [Entry]
toList f@(File _ _) = [f]
toList d@(Directory name size entries) = Directory name size [] : concatMap toList entries

type MyState = [String]
type MyStateMonad = State MyState

prompt :: MyStateMonad [Entry]
prompt = do
    xss <- get
    if null xss then
        return []
    else do
        let (xs:xss') = xss
        put xss'
        let command = drop 2 xs
        if command == "cd .." then
            return []
        else
            if "ls" == command then do
                entries <- lsDir
                rest <- prompt
                return $ entries <> rest
            else if "cd " `isPrefixOf` command then do
                entries <- chDir command
                rest <- prompt
                return $ entries <> rest
            else
                error $ "Not implemented: " ++ xs

chDir :: String -> MyStateMonad [Entry]
chDir xs = do
    let dir = drop 3 xs
    entries <- prompt
    return [Directory dir (-1) entries]

lsDir :: MyStateMonad [Entry]
lsDir = do
    xss <- get
    let (files, xss') = break isCommand xss
    put xss'
    let files' = filter (not . isDir) files
    return $ map toFile files'
  where
    isCommand xs = head xs == '$'
    isDir xs = "dir " `isPrefixOf` xs
    toFile xs = let [size, name] = splitOn " " xs in File name (read size)
