module NPoly
    ( bb, alg, NPoly, mulPoly
    ) where
import Ring
import Data.Fix
import Data.Bifunctor

type NPoly = [Int] -> Int

nZero :: NPoly
nZero = const 0

nOne :: NPoly
nOne x = if all (0==) x then 1 else 0

alg :: RingF NPoly -> NPoly
alg RZero = nZero
alg ROne = nOne
alg (RAdd f g) = \x -> (f x) + (g x)
alg (RMul f1 f2) = mulPoly f1 f2

mulPoly :: NPoly -> NPoly -> NPoly
mulPoly f1 f2 = f
    where f x = sum $ zipWith (*) coef1 coef2
            where comb = bb x
                  for1 = comb
                  for2 = map (zipWith (-) x) for1
                  coef1 = map f1 for1
                  coef2 = map f2 for2

data PairF e = Pair (Maybe (Int, e))
type Pair = Fix PairF

instance Functor PairF where
    fmap f (Pair e) = Pair $ fmap (second f) e

cartesian :: [Int] -> [Pair] -> [Pair]
cartesian xs ys = [Fix $ Pair $ Just (x, y) | x <- xs, y <- ys]

pairAlg :: PairF [Int] -> [Int]
pairAlg (Pair m) = case m of
                        Nothing -> []
                        Just (x, xs) -> x:xs

bb :: [Int] -> [[Int]]
bb xs = map (cata pairAlg) $ foldr cartesian [Fix $ Pair Nothing] $ map (\x -> [0..x]) xs