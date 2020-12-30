-- the string should be in the form of `$2,345,678.99` and can possibly have leading zeros
convert :: String -> Float
convert x = read stripped::Float
    where
        stripped = filter (/=',') (filter (/='$') x)

