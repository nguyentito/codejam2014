{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import Data.Word
import Data.Bits
import qualified Data.Set as Set
import System.IO

main = do
  cases <- parse . lines <$> hGetContents stdin
  forM_ (zip [(1::Int)..] (map solve cases)) $ \(i, sol) ->
    putStrLn $ "Case #" ++ show i ++ ": " ++ show sol

parse :: [String] -> [(Word32, Word32, Word32)]
parse (numCasesStr:rest) = map parse' $ take numCases rest
  where numCases = read numCasesStr
        parse' line = let [sa, sb, sk] = words line in
                      (read sa, read sb, read sk)

-- Very naive solution
solve :: (Word32, Word32, Word32) -> Int
solve (a, b, k) = length [ () | x <- [0..(a-1)], y <- [0..(b-1)], x .&. y < k]

