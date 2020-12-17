reverseList [] = []
reverseList (h:t) = reverseList t ++ [h]