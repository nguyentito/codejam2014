{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import qualified Data.Set as Set
import System.IO

main = do
  cases <- parse . tail . lines <$> hGetContents stdin
  forM_ (zip [(1::Int)..] (map solve cases)) $ \(i, (d, w)) ->
    putStrLn $ "Case #" ++ show i ++ ": " ++ show d ++ " " ++ show w

parse (str_n:naomi:ken:rest) = (f naomi, f ken) : parse rest
  where n :: Int
        n = read str_n
        f :: String -> [Double]
        f = map read . take n . words
    
parse _ = []

solve :: ([Double], [Double]) -> (Int, Int)
solve (naomiList, kenList) =
  (deceitful 0 naomiInit kenInit, war 0 naomiInit kenInit)
  where naomiInit = Set.fromList naomiList
        kenInit   = Set.fromList kenList
        war !ctr !naomi !ken
          | Set.null naomi = ctr
          | Just x <- Set.lookupGT (Set.findMax naomi) ken =
            war ctr (Set.deleteMax naomi) (Set.delete x ken)
          | otherwise =
              war (ctr+1) (Set.deleteMax naomi) (Set.deleteMin ken)
        deceitful !ctr !naomi !ken
          | Set.null naomi = ctr
          | Just x <- Set.lookupGT (Set.findMin ken) naomi =
            deceitful (ctr+1) (Set.delete x naomi) (Set.deleteMin ken)
          | otherwise =
              deceitful ctr (Set.deleteMin naomi) (Set.deleteMax ken)

