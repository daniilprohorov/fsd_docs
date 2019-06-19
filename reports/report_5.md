# Отчёт 5 
### Степик 1.5 
## ДЕБИЛЬНОЕ ЗАДАНИЕ!! ( отрицательные числа Фибоначч ) 
<details>
<summary>КОД</summary>
<p>
  
```haskell
fibr 0 = 0
fibr 1 = 1
fibr n = fibr (n-1) + fibr (n-2)


fibl 0 = 0
fibl 1 = 1
fibl n = fibl (n+2) - fibl (n+1)

fibonacci :: Integer -> Integer
fibonacci n
        | n < 0     = fibl n
        | otherwise = fibr n

```
<p>
</details>

**Вывод:** Дебильное задание, но я в принципе все уже написал в чатике, что это не задание, а отстой, ну вы поняли, чисто FACTS!!

## Быстрое Фибоначчи 
<details>
<summary>КОД</summary>
<p>
  
```haskell
fibonacci n
        | n == 0    = 0
        | n > 0     = fibr (n+1) [1, 0]
        | otherwise = fibl (n-1) [1, 0]

fibr 1 _ = 1
fibr n lst
  | n > length lst = fibr n (head1 + head2 : lst)
  | otherwise      = head lst
  where
      head1 = head lst
      head2 = head $ tail lst



fibl (-1) _ = 1
fibl n lst
  | abs n > length lst = fibl n (head2 - head1 : lst)
  | otherwise          = head lst
  where
      head1 = head lst
      head2 = head $ tail lst
```
<p>
</details>

**Вывод:** В принципе очень похожее решение делал для каты трибоначчи, чтобы все быстро работало, так что решение не вызвало каких-то затруднений, написал ппока ехал из Девяткино на Ленинский проспект.  

