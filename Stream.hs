module Stream where
import qualified Prelude as P
import Prelude (Int, Char, String, Bool(..), Show(..), (++), Eq(..), Ord(..), Ordering(..), Num(..), otherwise)
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
    | n == 0    = a
    | otherwise =(!!) b (n - 1)

-- Берёт из потока несколько первых элементов: 
take :: Int -> Stream a -> [a]
take n (a :& b)
    | n <= 0    = []
    | otherwise = a : take (n - 1) b

instance Show a => Show (Stream a) where 
    show xs = showInfinity (show (take 5 xs)) 
        where showInfinity x = P.init x ++ "..."

-- Преобразование потока
map :: (a -> b) -> Stream a -> Stream b
map f (a :& b) = f a :& map f b

-- Фильтрация потока
filter :: (a -> Bool) -> Stream a -> Stream a
filter f (a :& b)
    | f a       = a :& filter f b
    | otherwise = filter f b 

-- zip-ы для потоков:
zip :: Stream a -> Stream b -> Stream (a, b)
zip (ha :& ta) (hb :& tb) = (ha, hb) :& zip ta tb

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (ha :& ta) (hb :& tb) = f ha hb :& zipWith f ta tb 

iterate :: (a -> a) -> a -> Stream a
iterate f a = a :& iterate f (f a)

infix 0 :&

instance (Num a, Ord a) => Num (Stream a) where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
    abs = map abs
    signum = map signum
    negate = map signum
