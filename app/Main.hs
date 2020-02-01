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
    let multiplied2 = cata NPoly.alg $ Fix (RMul (Fix ROne) (Fix RZero))
    let negated = cata NPoly.alg $ Fix (RMul (Fix ROne) (Fix (RNeg (Fix ROne))))
    print $ square added
    print $ square multiplied
    print $ square multiplied2
    print $ square negated

square :: NPoly.NPoly -> [[Int]]
square poly = map f [0..2]
    where f :: Int -> [Int]
          f x = map (g x) [0..2]
            where g :: Int -> Int -> Int
                  g x y = poly [x,y]