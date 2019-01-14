module RainbowAssign where

import System.Random
import Data.List
import Data.Int
import qualified Data.Map as Map
import Data.Bits
import Data.Char        ( ord )

type Hash = Int32
type Passwd = String

hashString :: String -> Int32
hashString = foldl' f golden
   where f m c = fromIntegral (ord c) * magic + hashInt32 m
         magic = -559038737
         hashInt32 :: Int32 -> Int32
         hashInt32 x = mulHi x golden + x
         golden :: Int32
         golden = 1013904242
         mulHi :: Int32 -> Int32 -> Int32
         mulHi a b = fromIntegral (r `shiftR` 32)
           where r :: Int64
                 r = fromIntegral a * fromIntegral b

pwHash :: Passwd -> Hash
pwHash = hashString

randomList :: (Random t) => (t, t) -> Int -> IO [t]
randomList (a,b) n =  do
    seed <- newStdGen
    let randomListGen (a0,b0) n0 = take n0 . unfoldr (Just . (randomR (a0,b0)))
    let rs = randomListGen (a,b) n seed
    return rs

randomPasswords :: Int -> Int -> Int -> IO [[Char]]
randomPasswords nLetters len count = do
  let lastLetter = (iterate succ 'a')!!(nLetters-1)
  chars <- randomList ('a',lastLetter) (len*count)
  return [take len $ drop (i*len) chars | i <- [0..count-1]]

toLetter :: Int -> Char
toLetter n = toEnum (97 + n)

type RainbowFunction = Int -> [Passwd] -> Map.Map Hash Passwd
buildTable :: RainbowFunction -> Int -> Int -> Int -> Int -> IO (Map.Map Hash Passwd)
buildTable rainbowTable nLett len w h = do
  pws <- randomPasswords nLett len h
  return $ rainbowTable w pws

writeTable :: Map.Map Hash Passwd -> FilePath -> IO ()
writeTable table filename = writeFile filename tableStr
  where tableStr = show $ Map.toList table

readTable :: FilePath -> IO (Map.Map Hash Passwd)
readTable fn = do
  tableStr <- readFile fn
  let tableData = read tableStr
  return (Map.fromList tableData)
