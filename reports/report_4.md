# Отчёт 4 
### Дополнительная ката №2
## Help the bookseller ! 
https://www.codewars.com/kata/help-the-bookseller
<details>
<summary>КОД</summary>
<p>
  
```haskell
module Codewars.Kata.Bookseller where
import Codewars.Kata.Bookseller.Types

-- data Stock    = Stock String Int deriving (Show, Eq)
stocklist :: [Stock] -> [Char] -> [(Char, Int)]
stocklist [] _      = []
stocklist _  []     = []
stocklist st (x:xs) = (x, sum $ map count $ filter (ff x) st) : stocklist st xs
                  where
                    ff pattern (Stock (letter:_) _) = letter == pattern
                    count (Stock _ n) = n
```
<p>
</details>

**Вывод:** как-то придумал с 1 раза нормальное решение и оно даже заработало! Чудеса! Да и внешне мне нравится как выглядит! Все хорошо в общем! Но конечно из-за сессии сейчас немного напряг, так что не так часто что-то делаю по хаскелю, но это временно, так что не переживайте, все хорошо! 

Кстати по поводу той задачи с перестановками, у меня горит уже от нее,  я придумла другой алгоритм, котоырй как бы как дерево проходит всю строчку, но там возникли еще некоторые проблемки, из-за чего все медленно работает и я опять не могу ее сдать!!! При этом сдать то очень хочется, как это так! Ну в общем такие вот пока дела. 
