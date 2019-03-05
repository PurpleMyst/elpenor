module Zipper(Zipper, fromList, toList, zipLeft, zipRight, getOffset, setOffset, current) where

data Zipper a = Zipper [a] [a] deriving Show

fromList :: [a] -> Zipper a
fromList = flip Zipper []

toList :: Zipper a -> [a]
toList (Zipper l r) = l ++ r

zipLeft :: Zipper a -> Zipper a
zipLeft (Zipper l r) = Zipper (head r : l) (tail r)

zipRight :: Zipper a -> Zipper a
zipRight (Zipper l r) = Zipper (tail l) (head l : r)

current :: Zipper a -> a
current (Zipper l _) = head l

getOffset :: Zipper a -> Int
getOffset (Zipper l _) = length l

setOffset :: Zipper a -> Int -> Zipper a
setOffset z n
    | d < 0     = goRight z (abs d)
    | otherwise = goLeft z d
  where
    d = n - getOffset z

    goRight z 0 = z
    goRight z n = goRight (zipRight z) (n - 1)

    goLeft z 0 = z
    goLeft z n = goLeft (zipLeft z) (n - 1)
