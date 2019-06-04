# Отчёт 2
## Tribonacci sequence
https://www.codewars.com/kata/tribonacci-sequence
<details>
<summary>КОД</summary>
<p>
  
```haskell
module Tribonacci where

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n = reverse $ tri n [c, b, a]

tri 0 _ = []
tri 1 l = drop 2 l
tri 2 l = drop 1 l
tri 3 l = l
tri n l = tri (n-1) (sum ( take 3 l) : l)
```

</p>
</details>

**Вывод:** интересная задача, хотя я наверное потратил на неё 4 часа чистого времени + еще время на подумать, хотя оказалось, что я просто не учитываю, когда у меня n < 3, но все хорошо и я все учел, поэтому все получилось! Кстати думал что задача сложнее. Сначала думал использовать функцию tribonacci, но как-то я не придумал как с ней сделать нормальную рекурсию, так что создал еще одну функцию *tri* и просто внедрил ее куда надо. Мне кажется решение достаточно хорошее именно по скорости, так как не разрастается как огромное дерево. 

## Title case
https://www.codewars.com/kata/title-case

<details>
<summary>КОД</summary>
<p>  

```haskell
module TitleCase (titleCase) where

import           Data.Char (toLower, toTitle)

titleCase :: String -> String -> String
titleCase minor title = unwords $ reverse $ convert (words $ lower minor) (( reverse . words . lower) title)

convert :: [String] -> [String] -> [String]
convert _ [] = []
convert minorLst [x] = [title x]
convert minorLst (x:xs)
                    | x `elem` minorLst = lower x : convert minorLst xs
                    | otherwise = title x : convert minorLst xs

lower :: String -> String
lower = map toLower

title :: String -> String
title str = (toTitle . head) l : tail l where l = lower str
```
</p>
</details>

**Вывод:** решил не так прямо уж быстро, но решение в голову пришло почти сразу, поборолся немного с компилятором. Конечно колхоз в titleCase, можно было как-нибудь и получше все это распределить, но так тоже работает, хотя конечно и не очень то это читабельно. Так что потом постараюсь почитабельнее сделать. Еще не очень круто,что title аргумент собвпадает по называнию с title, которая функция. В остальном вроде все не плохо. Идея переворачивать список мне показалась достаточно логичной. 


Завтра наверное сделаю еще 3 каты. Ну одну на самом деле я уже сделал, но выложу ее в завтрашнем отчете 
