module Where where

-- let x = 5 in x
val1 = x
    where x = 5

-- let x = 5 in x * x
val2 = x * x
    where x = 5

-- let x = 5; y = 6 in x * y
val3 = x * y
    where x = 5
          y = 6

-- let x = 3; y = 1000 in x + 3
val4 = x + 3
    where x = 3
          y = 1000

-- let x = 3; y = 1000 in x * 3 + y
val5 = x * 3 + y
    where x = 3
          y = 1000

-- let y = 10; x = 10 * 5 + y in x * 5
val6 = x * 5
    where y = 10
          x = 10 * 5 + y

-- let x = 7; y = negate x; z = y * 10 in z / x + y
val7 = z / x + y
    where x = 7
          y = negate x
          z = y * 10

waxOn = x * 5
    where z = 7
          y = z + 8
          x = y ^ 2

triple x = x * 3

waxOff x = triple x