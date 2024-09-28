{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module StrictTypes.Tuples where
import Data.Data (Data)
import GHC.Generics ( Generic, Generic1 )
import Control.Lens (Field1 (_1), Field2 (_2), (^.), (<&>))

data Pair a b = Pair !a !b
    deriving (Eq,Ord,Show,Read,Data,Generic,Generic1)

fst:: Pair a b -> a
fst (Pair a _) = a

snd:: Pair a b -> b
snd (Pair _ b) = b

instance (Field1 (Pair a b) (Pair a' b) a a') where
    _1 f (Pair a b) = f a <&> \a' -> Pair a' b 

instance (Field2 (Pair a b) (Pair a b') b b') where
    _2 f (Pair a b) = f b <&> \b' -> Pair a b' 

something:: IO()
something = do
    let p = Pair 1 "A"
    let val = p ^. _1
    print ""
