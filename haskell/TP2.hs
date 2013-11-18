-- Operation on Generic Lists.
-- http://hackage.haskell.org/package/base-4.2.0.1/docs/Data-List.html

-- | Extract the first element of a list, which must be non-empty.
myHead :: [a] -> a
myHead (x:_) = x

-- | Extract the elements after the head of a list, which must be
-- non-empty.
myTail :: [a] -> [a]
myTail (_:xs) = xs

-- | Append two lists.
myAppend :: [a] -> [a] -> [a]
myAppend (x:xs) ys = x : myAppend xs ys
myAppend []     ys = ys

myAppend' :: [a] -> [a] -> [a]
myAppend' (x:xs) ys = x : suite
    where
      suite = myAppend' xs ys
myAppend' []     ys = ys

myAppend'' :: [a] -> [a] -> [a]
myAppend'' (x:xs) ys = let suite = myAppend'' xs ys
                       in x : suite
myAppend'' []    ys = ys

-- | Retun all elements of a list except the last one.
myInit :: [a] -> [a]
myInit [x]    = []
myInit (x:xs) = x : myInit xs

-- | Extract the last element of a list, which must be finite and
-- non-empty.
myLast :: [a] -> a
myLast [x]    = x
myLast (_:xs) = myLast xs

-- | Test whether a list is empty.
myNull :: [a] -> Bool
myNull [] = True
myNull _  = False

-- | O(n). length returns the length of a finite list as an Int.
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength(xs)

-- | reverse xs returns the elements of xs in reverse order. xs must
-- be finite.
myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse(xs) `myAppend` [x]
myReverse []     = []

-- iteratif, comparer les complexites experimentalement
myReverse' :: [a] -> [a]
myReverse' l = rev l []
    where
      rev []     l' = l'
      rev (x:xs) l' = rev xs (x:l')

-- | Concatenate a list of lists.
myConcat :: [[a]] -> [a]
myConcat (xs:xss) = xs `myAppend` myConcat xss
myConcat []       = []

-- | and returns the conjunction of a Boolean list.
myAnd :: [Bool] -> Bool
myAnd (x:xs) =  x && myAnd(xs)
myAnd []     = True

-- | or returns the disjunction of a Boolean list.
myOr ::  [Bool] -> Bool
myOr (x:xs) =  x || myOr(xs)
myOr []     = False

-- | The product function computes the product of a finite list of
-- numbers.
myProduct :: (Num a) => [a] -> a
myProduct (x:xs) = x * myProduct(xs)
myProduct []     = 1

-- | take n, applied to a list xs, returns the prefix of xs of length
-- n, or xs itself if n > length xs.
myTake :: Int -> [a] -> [a]
myTake n _      | n <= 0 = []
myTake _ []              = []
myTake n (x:xs)          = x : myTake (n-1) xs

-- | drop n xs returns the suffix of xs after the first n elements, or
-- [] if n > length xs.
myDrop :: Int -> [a] -> [a]
myDrop n l      | n <= 0 = l
myDrop _ []              = []
myDrop n (_:xs)          = myDrop (n-1) xs

-- | The insert function takes an element and a list and inserts the
-- element into the list at the last position where it is still less
-- than or equal to the next element.
myInsert :: (Ord a) => a -> [a] -> [a]
myInsert e (x:xs) | e > x     = x : myInsert e xs
                  | otherwise = e : x : xs
myInsert e []                 = [e]

-- | The sort function.
mySort :: (Ord a) => [a] -> [a]
mySort l =  srt l []
    where
      srt (x:xs) l' = srt xs (myInsert x l')
      srt []     l' = l'

-- Higher-order function.

-- In mathematics and computer science, a higher-order function (also
-- functional form, functional or functor) is a function that does at
-- least one of the following:
-- * take one or more functions as an input
-- * output a function
-- http://en.wikipedia.org/wiki/Higher-order_function

-- | takeWhile, applied to a predicate p and a list xs, returns the
-- longest prefix (possibly empty) of xs of elements that satisfy p.
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p (x:xs) | p x       = x : myTakeWhile p xs
                     | otherwise = []
myTakeWhile p [] = []

-- | map f xs is the list obtained by applying f to each element of
-- xs.
myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = (f x) : myMap f xs
myMap f [] = []

-- | The subsequences function returns the list of all subsequences of
-- the argument.
mySubsequences :: [a] -> [[a]]
mySubsequences = undefined
