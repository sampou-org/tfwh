> -- |
> -- 6桁以下の整数を英語読みに変換
> --
> module TFwH.NumbersIntoWords (convert) where
>
> units, teens, tens :: [String]
> units = [ "zero", "one", "two", "three", "four", "five"
>         , "six", "seven", "eight", "nine"
>         ]
> teens = [ "ten", "eleven", "twelve", "thirteen", "fourteen"
>         , "fifteen", "sixteen", "seventeen", "eighteen"
>         , "nineteen"
>         ]
> tens  = [ "twenty", "thirty", "forty", "fifty", "sixty"
>         , "seventy", "eighty", "ninety"
>         ]
>
> convert1 :: Int -> String
> convert1 n = units !! n
>
> digits2 :: Int -> (Int, Int)
> digits2 n = n `divMod` 10
>
> convert2 :: Int -> String
> convert2 = combine2 . digits2
>
> combine2 :: (Int, Int) -> String
> combine2 (t, u)
>   | t == 0           = units !! u
>   | t == 1           = teens !! u
>   | 2 <= t && u == 0 = tens !! (t - 2)
>   | 2 <= t && u /= 0 = tens !! (t - 2) ++ "-" ++ units !! u
>
> convert3 :: Int -> String
> convert3 n
>   | h == 0    = convert2 t
>   | t == 0    = units !! h ++ " hundred"
>   | otherwise = units !! h ++ " hundreds and " ++ convert2 t
>   where
>     (h, t) = n `divMod` 100
>
> convert6 :: Int -> String
> convert6 n
>   | m == 0    = convert3 h
>   | h == 0    = convert3 m ++ " thousand"
>   | otherwise = convert3 m ++ " thousand" ++ link h ++ convert3 h
>   where
>     (m, h) = n `divMod` 1000
>
> link :: Int -> String
> link h = if h < 100 then " and " else " "
>
> -- |
> -- 数を英単語に変換する
> --
> -- >>> convert 308000
> -- "three hundreds and eight thousand"
> -- >>> convert 369027
> -- "three hundreds and sixty-nine thousand and twenty-seven"
> -- >>> convert 369401
> -- "three hundreds and sixty-nine thousand four hundreds and one"
> --
> convert :: Int -> String
> convert = convert6

