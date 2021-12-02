# AoC 2021 Day 2

Pretty straightforward: смоделировали данные, применили. 

Вопросики по ходу возникли такие:
- Для тестов понадобились multiline strings, а у них тут в Хаскеле их нет: либо сторонние библиотеки странные, либо конкатинровать из листа, либо обмазываться эскейпами:
  ```haskell
  let input = "forward 5\n\
              \down 5\n\
              \forward 8\n\
              \up 3\n\
              \down 8\n\
              \forward 2"
  ```
- Разница между foldr, foldl, foldl' и прочими фолдами
- Работа с data record'ами, их паттерн-матчинг, доставание по ключу. Пока пришлось избежать и обращаться по-простому. UPD: почитал, применил:
  ```haskell
  -- Такое: 
  apply2 pos@Position{..} (Forward x) = pos{xPos = xPos + x, depth = depth + aim * x}
  apply2 pos@Position{..} (Down x) = pos{aim = aim + x}
  apply2 pos@Position{..} (Up x) = pos{aim = aim - x}
    
  -- Вместо такого:
  apply2 (Position xP d a) (Forward x) = Position (xP + x) (d + a * x) a
  apply2 (Position xP d a) (Down x) = Position xP d (a + x)
  apply2 (Position xP d a) (Up x) = Position xP d (a - x)
  ```