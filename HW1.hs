-- CptS 355 - Spring 2020 Assignment 1
-- Philip GeLinas

module HW1
     where

-- 1a. exists
exists :: Eq t => t -> [t] -> Bool
exists x (y:ys) | x == y = True
                | ys == [] = False
                | otherwise = exists x ys

-- 1b. type for exists
-- The type for our 'exists' function is
-- exists :: Eq t => t -> [t] -> Bool 
-- and not 
-- exists :: t -> [t] -> Bool 
-- because we must use the == operator, which is contained by the Eq typeclass. 

-- 1.c countInList
countInList :: (Num p, Eq t) => t -> [t] -> p
countInList x [] = 0
countInList x (y:ys) | x == y = 1 + countInList x ys
                     | otherwise = countInList x ys

-- 2. listDiff
listDiff :: Eq a => [a] -> [a] -> [a]
listDiff x [] = x
listDiff x y = listDiffHelp x y []

listDiffHelp :: Eq a => [a] -> [a] -> [a] -> [a]
listDiffHelp [] y z = z
listDiffHelp (x:xs) y z | (countInList x (x:xs)) - (countInList x y) > 0 = listDiffHelp xs y (x:z)
                        | otherwise = listDiffHelp xs y z

-- 3. firstN



-- 4. busFinder



-- 5. cumulativeSums




-- 6. groupNleft



