# Отчёт 3 
### Дополнительная ката №1
## Consecutive letters 
https://www.codewars.com/kata/consecutive-letters
<details>
<summary> **КОД** </summary>
```haskell
module ConsecutiveLetters where 
import Data.List (nub, isPrefixOf, sort, dropWhile)


solve :: String -> Bool
solve xs = if nub xs == xs 
then isPrefixOf (sorted) (dropWhile (< first) ['a'..'z']) 
else False
where 
sorted = sort xs
first = head sorted
```
</details>
**Вывод:** на самом деле решается в одну строчку, но я не додумался, так что вот накрутил тут чудеса всякие, хотя это также работает. Вообще это моя первая ката на кодеварс, т.к. я когда только зашел, даже не зарегестрировался еще, сразу ее увидел и опять думал в метро, ну потом придумал и решил все достаточно простым способом, хотя и длинным. 

## Бесконечные структуры
https://www.codewars.com/kata/functional-streams
<details>
<summary> **КОД** </summary>
```haskell
module Stream where

import Control.Arrow
import Control.Applicative

import Stream.Internal

-- Defined in Stream.Internal:
--     data Stream a = a :> Stream a
--     infixr :>

-- | Get the first element of a stream.
headS :: Stream a -> a
headS (x :> _) = x

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS (_ :> xs) = xs

-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS value = value :> repeatS value

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :> iterateS f (f x)

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS s@(x:xs) = cycleS' s s
                where
                  cycleS' [] s@(x:xs) = x :> cycleS' xs s
                  cycleS' (x:xs) s    = x :> cycleS' xs s

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS x = x :> fromS (x+1)

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = x :> fromStepS (x + s) s

-- }}}


-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = f x $ foldrS f xs

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (x :> xs)
                | p x       = x :> filterS p xs
                | otherwise = filterS p xs

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS n (x :> xs)
            | n <= 0 = []
            | otherwise = x : takeS (n-1) xs


-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS i s@(x :> xs)
              | i <= 0    = s
              | otherwise = dropS (i-1) xs

-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS i s = (takeS i s, dropS i s)

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (x :> xs) (y :> ys) = f x y :> zipWithS f xs ys

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS (x :> xs) (y :> ys) = (x, y) :> zipS xs ys

instance Functor Stream where
    -- fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
    -- pure :: a -> Stream a
    pure x = repeatS x

    -- (<*>) :: Stream (a -> b) -> Stream a -> Stream b
    (<*>) (f:>fs) (x:>xs) = f x :> (<*>) fs xs

-- | The stream of fibonacci numbers.
fibS :: Stream Integer
fibS = 0 :> 1 :> zipWithS (+) fibS (tailS fibS)

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = filterS isPrime (2 :> fromStepS 3 2)

isPrime x = isPrime' x (x-1)
      where
        isPrime' x 1 = True
        isPrime' x d
                  | x `mod` d == 0 = False
                  | otherwise      = isPrime' x (d-1)
                  
```
</details>
**Вывод:** Ну что тут можно сказать, после всяких легких кат это конечно нормальная такая задачка была, особенно если учесть, что я до этого не работал с бесконечными структурами данных, но fmap с <*> конечно же использовал, куда без этого. В начале конечно поломал мозг, от того что оно как все происходит и не провалюсь ли я сейчас в бесконечную рекурсию, но потом как-то привык и все стало нормально, некоторые функции давались легко, а над некоторыми пришлось попотеть, как например над cycleS, ухх, очень долго уж я думал и сделал далеко не с первого раза, потому что как-то хотелось без вспомогательных функций, даже посмотрел как в бесконечных списках, а там просто ++ используется. Ну и пришлось делать с доп функцией, которая в where лежит, в принципе не страшно, но такое себе. Следующим сложным элементом было написать бесконечную последовательность Фибоначчи, я наверное часа 2 потратил, был где-то около решения, но отчаялся и посмотрел как сделано в бесконечных листах, ну и там легко было переделать под поток. А вот с простыми числами проблем как-то и не возникло, да и там подсказка была про фильтр, хотя я это понял и до подсказки. Также нашел как сделать небольшую оптимизацию ( можно после тройки всегда прибавлять по 2, а не по 1 ), возможно там где-то это и не так должно рабоать, но тесты все были пройдены, значит моя гипотеза с прибавлением по 2 была верна. Кстати проверил, почему-то разница в 1 секунду примерно, видимо там большое количество простых чисел создается ( я проверил нескольно раз => что это не погрешность ). Ну и функцию isPrime тоже было легко написать! 


Так что вот такой вот сегодня день был, по факту только одну задачку про бесконечные структуры и сделал. Завтра буду делать экономику для универа, так что не знаю сколько чего получится сделать)). Еще сегодня читал Typeclassopedia, или как там она называется, прочитал про функторы, мозг напрягся в принципе, но вроде что-то понял, по крайней мере мне это сегодня понадобилось в fmap, pure и <*>. В общем день прошел отлично!

Еще кстати заманил 2 людей, своих бывших одногруппников ( и такое бывает, когда переводишься из одного университета в другой ) из РнД, они конечно не супер топ ФП Боги, но тоже пусть поучатся, может понравится, по крайней мере то что они согласились на такую авантюру - уже неплохо.  А теперь можно и спать идти, а вам доброе утро!!


