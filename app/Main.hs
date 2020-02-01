module Main where

import Data.Fix
import qualified Lib
import qualified NPoly
import Ring


main :: IO ()
main = do
    print $ cata Lib.alg $ Fix (RAdd (Fix ROne) (Fix ROne))
    let added = cata NPoly.alg $ Fix (RAdd (Fix ROne) (Fix ROne))
    let multiplied = cata NPoly.alg $ Fix (RMul (Fix ROne) (Fix ROne))
    print $ square $ cata NPoly.alg $ Fix ROne
    print $ square added
    print $ square multiplied

square :: NPoly.NPoly -> [[Int]]
square poly = map f [0..3]
    where f :: Int -> [Int]
          f x = map (g x) [0..3]
            where g :: Int -> Int -> Int
                  g x y = poly [x,y]
