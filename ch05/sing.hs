module Sing where -- was `sing`

fstString :: [Char] -> [Char] -- was `++`
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char] -- was `Char`
sndString x = x ++ " over the rainbow"

-- replace > with < to sing the other song
sing = if (x > y) then fstString x else sndString y -- was `or`
    where x = "Singin" -- indentation was incorrect
          y = "Somewhere" -- was `x`