module Stream where
import Prelude
import Nat
data Stream a = a :& Stream a
    deriving(Show)

constStream :: a -> Stream a
constStream a = a :& constStream a

nats :: Nat -> Stream Nat 
nats a = a :& nats (Succ a)

nums :: Integer -> Stream Integer
nums a = a :& nums (a + 1)

-- Первый элемент потока
head :: Stream a -> a
head (a :& _) = a

-- instance Num Stream where
--     (+) (a :& constStream a) (b :& constStream b) = a + b