module Zipper(Zipper, fromList, left, right , zipLeft, zipRight, getOffset, setOffset, current, maybeCurrent) where

data Zipper a = Zipper [a] [a] deriving Show

fromList :: [a] -> Zipper a
fromList = Zipper []

left :: Zipper a -> [a]
left (Zipper l _) = l

right :: Zipper a -> [a]
right (Zipper _ r) = reverse r

zipRight :: Zipper a -> Zipper a
zipRight (Zipper l (x:r)) = Zipper (x : l) r
zipRight (Zipper _ [])    = error "can't zip right"

zipLeft :: Zipper a -> Zipper a
zipLeft (Zipper (x:l) r) = Zipper l (x : r)
zipLeft (Zipper []    _) =  error "can't zip left"

current :: Zipper a -> a
current (Zipper _ (x:_)) = x
current (Zipper _ [])    = error "can't get current"

maybeCurrent :: Zipper a -> Maybe a
maybeCurrent (Zipper _ (x:_)) = Just x
maybeCurrent (Zipper _ [])    = Nothing

getOffset :: Zipper a -> Int
getOffset (Zipper l _) = length l

setOffset :: Int -> Zipper a -> Zipper a
setOffset n z
    | d < 0     = goLeft  (abs d) z
    | otherwise = goRight d       z
  where
    d = n - getOffset z

    goLeft 0 z'   = z'
    goLeft n' z'  = goLeft (n' - 1) (zipLeft z')
    goRight 0 z'  = z'
    goRight n' z' = goRight (n' - 1) (zipRight z')
