module Lib
    ( alg
    ) where
import Ring

take' :: Int -> [Int] -> [Int]
take' n l = if n > 0 then case l of [] -> replicate n 0
                                    (x:xs) -> x:(take' (n-1) xs)
                     else []
degree :: [Int] -> Int
degree xs = max 0 $ length xs - 1

alg :: RingF [Int] -> [Int]
alg RZero = []
alg ROne = [1]
alg (RNeg x) = map negate x
alg (RAdd (x:xs) (y:ys)) = (x+y):(alg $ RAdd xs ys)
alg (RAdd [] y) = y
alg (RAdd x []) = x
alg (RMul x y) = let f :: Int -> Int
                     f n = let xs = take' (n+1) x
                               ys = reverse $ take' (n+1) y in
                                   sum $ zipWith (*) xs ys
                     in
                         map f [0..(degree x + degree y)]