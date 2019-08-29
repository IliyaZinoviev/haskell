module Stream where
import Prelude (Int, Char, String, Show(..), (++), Eq(..), Ord(..), Ordering(..), Num(..), otherwise)
import qualified Prelude as P 
import Nat

data Stream a = a :& Stream a

nats :: Nat -> Stream Nat 
nats a = a :& nats (Succ a)

constStream :: a -> Stream a 
constStream a = a :& constStream a

nums :: Int -> Stream Int
nums a = a :& nums (a + 1)

-- Первый элемент потока 
head :: Stream a -> a
head (a :& _) = a
-- Хвост потока, всё кроме первого элемента 
tail :: Stream a -> Stream a
tail (_ :& a) = a
-- n-тый элемент потока 
(!!) :: Stream a -> Int -> a
(!!) (a :& b) n
    | n == 0 = a
    | otherwise =(!!) b (n - 1)
-- Берёт из потока несколько первых элементов: 
take :: Int -> Stream a -> [a]
take n (a :& b)
    | n == 0 = []
    | otherwise = a ++ take (n - 1) b

instance Show a => Show (Stream a) where 
    show xs = showInfinity (show (take 5 xs)) 
        where showInfinity x = P.init x ++ "..." 
