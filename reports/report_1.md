# Отчёт 1
## Is this a triangle
https://www.codewars.com/kata/is-this-a-triangle
<details>

    ```haskell
    module Codewars.Triangles where

    isTriangle :: Int -> Int -> Int -> Bool
    isTriangle a' b' c' = if (all (> 0) [a', b', c']) && ((p-a)*(p-b)*(p-c) > 0)
                          then True
                          else False
                       where 
                         a = fromIntegral a'
                         b = fromIntegral b'
                         c = fromIntegral c'
                         p = (a + b + c)/2
    ```

</details>
**Вывод:** задание было простое, но мне хотелось решить его как-нибудь по особенному, поэтому даже пока думал решение, уехал на 2 станции метро в другую сторону, ну ладно, бывает. Решение должно было быть намного лаконичнее, но там было ограничение на тип данных, так что пришлось впилить fromIntegral. 

## Disemvowel trolls
https://www.codewars.com/kata/disemvowel-trolls
<details>
<summary> КОД </summary>
<p>
```haskell
module Disemvowel where
import qualified Data.Set as Set

vowels = Set.fromList "aeiouAEIOU"

disemvowel :: String -> String
disemvowel str = filter (\c -> not $ Set.member c vowels) str
```
</p>
</details>
**Вывод:** использовал множества, в принципе задание было простое, правда я сначала не учел большие символы, а еще хорошо, что нагуглил, что q - не согласная)) 

## Highest and lowest
https://www.codewars.com/kata/highest-and-lowest
<details>
<summary> КОД </summary>
<p>
```haskell
module Kata (highAndLow) where
import Data.List (sort)
highAndLow :: String -> String
highAndLow input = result (last sorted) (head sorted)
                where
                  numbers = map (\x -> read x :: Int) (words input)
                  sorted = sort numbers
                  result a b = (show a) ++ " " ++ (show b)
```
</p>
</details>
**Вывод:** вроде как нормальное решение, по идее должно быть вполне быстрое, хотя это спорный вопрос, что быстрее min и max найти или отсортировать и взять последний и первый элемент. 

## Isograms
https://www.codewars.com/kata/isograms
<details>
<summary> КОД </summary>
<p>
```haskell
module Isogram where
import Data.List (nub)
import Data.Char (toLower)

isIsogram :: String -> Bool
isIsogram ""  = True
isIsogram str = (nub lowerStr) == lowerStr 
                where 
                  lowerStr = strToLower str
                  strToLower str = [ toLower c | c <- str]
```
</p>
</details>
**Вывод:** очень простая, использую встроенную библиотеку с ф-ей nub. 

## Split strings
https://www.codewars.com/kata/split-strings
<details>
<summary> КОД </summary>
<p>
```haskell
module Codewars.Kata.SplitStrings where

solution :: String -> [String]
solution []       = []
solution (a:[])   = [[a, '_']]
solution (a:b:[]) = [[a, b]]
solution (a:b:xs) = [[a, b]] ++ (solution xs)
```
</p>
</details>
**Вывод:** как я понял основной тут прикол на паттерн матчинг, но я тут больше со скобочками запутался и Char и [Char]


