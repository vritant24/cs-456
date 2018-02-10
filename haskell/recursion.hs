data IntList = 
        EmptyList
        -- | Cons {listHead :: Int, listTail :: IntList}
        | ConsList Int IntList
        deriving Show

data IntTree = 
        EmptyTree
        | ConsTree Int IntTree IntTree
        deriving Show

data Treen a = 
        EmptyTree
        | Cons a Tree a Tree a
        deriving Show


        makeList :: Int -> IntList
makeList 0 = ConsList 0 EmptyList
makeList n = ConsList n (makeList $ n - 1)

append :: IntList -> IntList -> IntList
append EmptyList y = y
append (ConsList x xs) y = ConsList x (append xs y)