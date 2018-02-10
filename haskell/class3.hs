-- INT
-- 3

-- String -> Char
f1 :: String -> Char
    f1 x = head(x)

-- Bool -> Bool -> Bool
f2 :: Bool -> Bool -> Bool
    f2 x y = x && y

-- Ill typed
-- f1 'c'
