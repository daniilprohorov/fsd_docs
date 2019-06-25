# Отчёт 6  
## Data compression using run-length encoding
https://www.codewars.com/kata/578bf2d8daa01a4ee8000046/
<details>
<summary>КОД</summary>
<p>
  
```haskell

-- не очень читабельно, но я что-то спешил, не знаю куда, но надо было очень
-- быстро решить эту кату

encode :: String -> String
encode []  = []
encode str = enc $ splitL str []

splitL :: String -> [String] -> [String]
splitL []     buff      = buff
splitL (s:ss) []        = splitL ss [[s]]
splitL (s:ss) ((hl:tl):tb)
            | hl == s   = splitL ss ((s:hl:tl):tb)
            | otherwise = splitL ss ([s]:(hl:tl):tb)

enc :: [String] -> String
enc bf = foldl1 (++) $ map (\b@(s:ss) -> (show . length) b ++ [s]) (reverse bf)

decode :: String -> String
decode str = postDecode $ decode' $ preDecode str

preDecode :: String -> String
preDecode [] = []
preDecode (s:ss)
  | s == 'e'   = '{' : preDecode ss
  | s == 'E'   = '@' : preDecode ss
  | otherwise  = s : preDecode ss

postDecode :: String -> String
postDecode [] = []
postDecode (s:ss)
  | s == '{'   = 'e' : postDecode ss
  | s == '@'   = 'E' : postDecode ss
  | otherwise  = s : postDecode ss

decode' :: String -> String
decode' []  = []
decode' str = replicate count letter ++ decode contStr
               where
                 readed = head (reads str :: [(Int, String)])
                 letter = head $ snd readed
                 count  = fst readed
                 contStr = tail $ snd readed

```
<p>
</details>

**Вывод:** В принципе интересное было задание, заинтрересовало, потому что один человек в чатике предлагал какое-то странное решение,  и я решил, что надо бы написать что-нибудь попроще и по логичнее, конечно по моему когду нельзя сказать, что он супер простой, но идея тут сама достаточно простая, для кодирования разделяем строку на подстроки, когда сменяется символ, а потом просто для каждой подстроки вызываем length и конкатенируем с головой этой подстроки, а для декодирования рекурсия которая использует reads. Вот тут то и появилась проблема, которую я сначала не учел - 1e1 рассматривалось как число с мантиссой, чего мне не хотелось, и вместо [(Int, String)] возвращалось [] что можно интерпретировать как ошибка, я сначла думал, что придется все переделывать, но потом просто заменял E на @ и все сработало! 
 
 Немного и дорешаю 2.5 на степике, пока все. 
