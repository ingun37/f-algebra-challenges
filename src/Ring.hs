module Ring
    ( RingF(..)
    ) where
import Data.Fix

data RingF a = RZero
             | ROne
             | RAdd a a 
             | RMul a a
             | RNeg a

instance Functor RingF where
    fmap f r = case r of
        RZero -> RZero
        ROne -> ROne
        RAdd a b -> RAdd (f a) (f b)
        RMul a b -> RMul (f a) (f b)
        RNeg a -> RNeg (f a)