-- greatest common denominator
-- 5 12 = 1

mygcd :: Integer -> Integer -> Integer
mygcd a 0 = a
mygcd a b = mygcd b (a `mod` b)