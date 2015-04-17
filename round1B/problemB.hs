{-# LANGUAGE BangPatterns, LambdaCase #-}

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.Map as Map
import Data.Word
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
solveNaive :: (Word32, Word32, Word32) -> Int
solveNaive (a, b, k) = length [ () | x <- [0..(a-1)], y <- [0..(b-1)], x .&. y < k]

-- Solution which basically counts the number of accepting paths
-- in a product of three automata on the alphabet {0,1}^2 testing for:
-- - map fst <= a
-- - map snd <= b
-- - map .&. <= k

data State = Success | Failure | Tentative
           deriving (Eq, Ord)

alphabet = [(x,y) | x <- [False,True], y <- [False,True]]

states = [Success, Failure, Tentative]

nextState Success _ _ = Success
nextState Failure _ _ = Failure
nextState Tentative True  False = Failure
nextState Tentative False True  = Success
nextState Tentative _     _     = Tentative

states3 = [(x,y,z) | x <- states, y <- states, z <- states]

solve :: (Word32, Word32, Word32) -> Int
solve (a, b, k) = count 31 Map.! (Tentative, Tentative, Tentative)
  where count (-1) = Map.insert (Success, Success, Success) 1
                     $ Map.fromList [(key, 0) | key <- states3]
        count n = Map.fromList [(key, f key) | key <- states3]
          where c = count (n-1)
                f (sa, sb, sk) = sum [ c Map.! next xx | xx <- alphabet]
                  where next (x, x') = (nextState sa x         (testBit a n),
                                        nextState sb x'        (testBit b n),
                                        nextState sk (x && x') (testBit k n))
