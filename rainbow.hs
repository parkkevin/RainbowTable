import RainbowAssign
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map


pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8
nLetters = 5
width = 40
height = 1000
filename = "table.txt"



pwReduce :: Hash -> [Char]
pwReduce n = toStr $ baseConverter (fromEnum n) nLetters


toStr :: [Int] -> [Char]
toStr [] = []
toStr (x:xs) = toLetter x : toStr xs


baseConverter :: Int -> Int -> [Int]
baseConverter n b = toBase (fromDec n b 1) pwLength


fromDec :: Int -> Int -> Int -> [Int]
fromDec 0 _ _ = take pwLength [0,0..]
fromDec (-1) b c
               | c >= pwLength = let r = b - 1 in take c [r,r..]
               | otherwise = fromDec (-1) b (c+1)
fromDec n b c = (fromDec quotient b (c+1)) ++ [remainder]
              where quotient = div n b
                    remainder = mod n b


toBase :: [Int] -> Int -> [Int]
toBase lst i = reverse . take i $ reverse lst ++ [0,0..]


rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable w pw = Map.fromList $ map (\x -> hashReduce w x x) pw


hashReduce :: (Eq a, Num a) => a -> Passwd -> Passwd -> (Hash, Passwd)
hashReduce 0 orig lst = ((pwHash lst), orig)
hashReduce w orig lst = hashReduce (w-1) orig (pwReduce . pwHash $ lst)


findPassword :: Map.Map Hash [Char] -> Int -> Hash -> Maybe Passwd
findPassword table w hval = findPw table w (isInTable table w hval hval) hval


findPw :: Map.Map Hash [Char] -> Int -> Maybe [Char] -> Hash -> Maybe Passwd
findPw table w rval orig
             | rval == Nothing = Nothing
             | w < 0 = Nothing
             | (pwHash . Maybe.fromJust $ rval) == orig = rval
             | otherwise = findPw table (w-1) getNext orig
                         where getNext = Just . pwReduce . pwHash . Maybe.fromJust $ rval


isInTable :: Map.Map Hash [Char] -> Int -> Hash -> Hash -> Maybe Passwd
isInTable table w hval orig
          | w < 0 = Nothing
          | Map.member hval table == True = falsePos
          | otherwise = isInTable table (w-1) (pwHash . pwReduce $ hval) orig
           where falsePos = isFalsePositive table w getRval tableSize  orig
                            where getRval = Map.lookup hval table
                                  tableSize = (Map.size table)-1


isFalsePositive :: Map.Map Hash [Char] -> Int -> Maybe Passwd -> Int -> Hash -> Maybe Passwd
isFalsePositive table w rval idx orig
           | idx < 0 = Nothing
           | findPw table w rval orig == Nothing = isFalsePositive table w nextRval (idx-1) orig
           | otherwise = rval
            where nextRval = Just (snd (Map.elemAt (idx-1) table))





generateTable :: IO ()
generateTable = do
             table <- buildTable rainbowTable nLetters pwLength width height
             writeTable table filename

-- test1 :: IO (Maybe Passwd)
-- test1 = do
--       table <- readTable filename
--       return (Map.lookup 0 table)

test2 :: Int -> IO ([Passwd], Int)
test2 n = do
          table <- readTable filename
          pws <- randomPasswords nLetters pwLength n
          let hs = map pwHash pws
          let result = Maybe.mapMaybe (findPassword table width) hs
          return (result, length result)

main :: IO ()
main = do
       generateTable
       res <- test2 1000
       print res
