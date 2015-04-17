{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Data.Ratio
import Numeric (readFloat)

parseLine :: String -> (Rational, Rational, Rational)
parseLine l = let [x,y,z] = map (fst.head.readFloat) . words $ l in (x, y, z)
  
main = do
  str <- readFile "B-large.in"
  forM_ (zip [(1::Int)..] (map (solve . parseLine) . tail . lines $ str)) $ \(i, res) ->
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (fromRational res :: Double)

solve (c, f, x) = loop 0 2
  where loop !t !v | dtCookie <= dtCookieFarm = t + dtCookie
                   | otherwise = loop (t + dtFarm) v'
          where dtFarm = c / v
                dtCookie = x / v
                v' = v + f
                dtCookieFarm = dtFarm + x/v'

