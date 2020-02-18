-- CptS 355 - Spring 2020 Assignment 2
-- Philip GeLinas

module HW2
     where


{- intersect & intersectTail & intersectAll - 22%-}

--intersect: returns the intersection between two lists, removing duplicates
intersect :: Eq a => [a] -> [a] -> [a]
intersect (x:xs) l2 | elem x xs = intersect xs l2
                        | elem x l2 = x : intersect xs l2
                        | otherwise = intersect xs l2
intersect l1 [] = []
intersect [] l2 = []


--intersectTail: returns the intersection between two lists, removing duplicates
intersectTail :: Eq a => [a] -> [a] -> [a]
intersectTail l1 l2 = intersectTailHelper l1 l2 []
                      where intersectTailHelper (x:xs) l2 acc | elem x acc = intersectTailHelper xs l2 acc
                                                                  | elem x l2 = intersectTailHelper xs l2 (x:acc)
                                                                  | otherwise = intersectTailHelper xs l2 acc
                            intersectTailHelper l1 [] acc = acc
                            intersectTailHelper [] l2 acc = acc


--intersectAll: returns the intersection between all lists of a 2D list, removing duplicates
intersectAll :: Ord a => [[a]] -> [a]
intersectAll [] = []
intersectAll l1 = foldr intersect (l1!!0) l1


{-2 - partition - 10%-}

--partition: returns a tuple of lists, the left of which contains all values that satisfy a given operation
--           and the second of which contains all values that do not satisfy the given operation
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition op iL = ((filter op iL), (filter' op iL))
                  where filter' :: (a -> Bool) -> [a] -> [a]
                        filter' op [] = []
                        filter' op (x:xs) | (op x) = filter' op xs
                                          | otherwise = x : filter' op xs


{- 3 - sumL, sumMaybe, and sumEither - 27% -}

--sumL: returns the overall sum of all numbers within a 2D list of numbers
sumL :: (Num b) => [[b]] -> b
sumL iL = foldr (+) 0 (map sum iL)


-- sumMaybe: returns the overall sum of all Maybe values within a 2D list of Maybe values
sumMaybe :: (Num a) => [[(Maybe a)]] -> Maybe a
sumMaybe iL = foldr addMaybe (Just 0) (map (foldr addMaybe (Just 0)) iL)
              where addMaybe :: (Num a) => Maybe a -> Maybe a -> Maybe a
                    addMaybe Nothing (Just y) = (Just y)
                    addMaybe (Just x) (Just y) = (Just (x + y))


data IEither  = IString String | IInt Int
                deriving (Show, Read, Eq)


-- sumEither: returns the overall sum of all IEither values within a 2D list of IEither values
sumEither :: [[IEither]] -> IEither
sumEither iL = foldr addEither (IInt 0) (map (foldr addEither (IInt 0)) iL)
               where addEither :: IEither -> IEither -> IEither
                     addEither (IInt x) (IInt y) = (IInt (x + y))
                     addEither (IString x) (IInt y) = (IInt ((getInt x) + y))
                     getInt :: String -> Int
                     getInt x = read x::Int


{-4 - depthScan, depthSearch, addTrees - 37%-}

data Tree a = LEAF a | NODE a (Tree a) (Tree a)
              deriving (Show, Read, Eq)
 

--depthScan: returns a list representation of the post-order traversal of a Tree
depthScan :: Tree a -> [a]
depthScan (LEAF x) = [x]
depthScan (NODE x t1 t2) = (depthScan t1) ++ (depthScan t2) ++ [x]


--depthSearch: returns the depth of the first instance of a given value within a Tree during a post-order traversal
depthSearch :: (Ord p, Num p, Eq a) => Tree a -> a -> p
depthSearch t val | elem val (depthScan t) = depthSearchHelper t val 1
                  | otherwise = -1
                    where depthSearchHelper :: (Ord p, Num p, Eq a) => Tree a -> a -> p -> p
                          depthSearchHelper (LEAF x) val depth | x == val = depth
                                                               | otherwise = -1
                          depthSearchHelper (NODE x t1 t2) val depth | elem val (depthScan t1) = depthSearchHelper t1 val (depth + 1)
                                                                     | elem val (depthScan t2) = depthSearchHelper t2 val (depth + 1)
                                                                     | otherwise = depth


--addTrees: returns the sum of two Trees
addTrees :: Num a => Tree a -> Tree a -> Tree a
addTrees (NODE x xl xr) (NODE y yl yr) = (NODE (x + y) (addTrees xl yl) (addTrees xr yr))
addTrees (NODE x xl xr) (LEAF y) = (NODE (x + y) xl xr)
addTrees (LEAF x) (NODE y yl yr) = (NODE (x + y) yl yr)
addTrees (LEAF x) (LEAF y) = (LEAF (x + y))


{- 5- Create two trees of type Tree. The height of both trees should be at least 4. Test your functions depthScan, depthSearch, addTrees with those trees. 
The trees you define should be different than those that are given.   -}
perfect = NODE 4 (NODE 7 (NODE 5 (LEAF 6) (LEAF 3)) (NODE 1 (LEAF 0) (LEAF 3))) (NODE 2 (NODE 8 (LEAF 7) (LEAF 7)) (NODE 9 (LEAF 2) (LEAF 5)))
lopsided  = NODE 1 (NODE 2 (NODE 3 (NODE 4 (LEAF 5) (LEAF 6)) (LEAF 7)) (LEAF 8)) (LEAF 9)