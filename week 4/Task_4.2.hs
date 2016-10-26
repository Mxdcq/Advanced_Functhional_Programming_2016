--Make a typeclass Small. To be a member of Small the class has to implement
--  isSmall function whose result is of type Bool (telling if a value is "small"
-- or not).
--Make Int a member of Small in such a way that a value is small if it is in
-- the range [-2,..,2] and make [] a member of Small in such a way that a value
-- is small if it has at most one element.

class Small a where
    isSmall :: a -> Bool

instance Small Int where
    isSmall (-2) = True
    isSmall (-1) = True
    isSmall 0 = True
    isSmall 1 = True
    isSmall 2 = True
    isSmall _ = False

instance Small [a] where
    isSmall x = if length x <= 1 then True else False