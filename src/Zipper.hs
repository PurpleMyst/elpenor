module Zipper(Zipper, fromList, left, right , zipLeft, zipRight, getOffset, setOffset, current) where

data Zipper a = Zipper [a] [a] deriving Show

fromList :: [a] -> Zipper a
fromList r = Zipper [] r

left :: Zipper a -> [a]
left (Zipper l _) = l

right :: Zipper a -> [a]
right (Zipper _ r) = reverse r

zipRight :: Zipper a -> Zipper a
zipRight (Zipper l r) = Zipper (head r : l) (tail r)

zipLeft :: Zipper a -> Zipper a
zipLeft (Zipper l r) = Zipper (tail l) (head l : r)

current :: Zipper a -> a
current (Zipper _ r) = head r

getOffset :: Zipper a -> Int
getOffset (Zipper l _) = length l

setOffset :: Int -> Zipper a -> Zipper a
setOffset n z
    | d < 0     = goLeft  (abs d) z
    | otherwise = goRight d       z
  where
    d = n - getOffset z

    goLeft 0 z  = z
    goLeft n z  = goLeft (n - 1) (zipLeft z)
    goRight 0 z = z
    goRight n z = goRight (n - 1) (zipRight z)
