-- Operation on Integer Lists.
-- http://hackage.haskell.org/package/base-4.2.0.1/docs/Data-List.html

-- | Extract the first element of a list, which must be non-empty.
myHead :: [Int] -> Int
myHead (x:_) = x

-- | Extract the elements after the head of a list, which must be
-- non-empty.
myTail :: [Int] -> [Int]
myTail (_:xs) = xs

-- | Append two lists.
myAppend :: [Int] -> [Int] -> [Int]
myAppend (x:xs) ys = x : myAppend xs ys
myAppend []     ys = ys

myAppend' :: [Int] -> [Int] -> [Int]
myAppend' (x:xs) ys = x : suite
    where
      suite :: [Int]
      suite = myAppend' xs ys
myAppend' []     ys = ys

myAppend'' :: [Int] -> [Int] -> [Int]
myAppend'' (x:xs) ys = let suite = myAppend'' xs ys
                       in x : suite
myAppend'' []    ys = ys

-- | Retun all elements of a list except the last one.
myInit :: [Int] -> [Int]
myInit [x]    = []
myInit (x:xs) = x : myInit(xs)

-- | Extract the last element of a list, which must be finite and
-- non-empty.
myLast :: [Int] -> Int
myLast [x]    = x
myLast (_:xs) = myLast(xs)

-- | Test whether a list is empty.
myNull :: [Int] -> Bool
myNull [] = True
myNull _  = False

-- | O(n). length returns the length of a finite list as an Int.
myLength :: [Int] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength(xs)

-- | reverse xs returns the elements of xs in reverse order. xs must
-- be finite.
myReverse :: [Int] -> [Int]
myReverse (x:xs) = myReverse(xs) `myAppend` [x]
myReverse []     = []

-- iteratif, comparer les complexites experimentalement
myReverse' :: [Int] -> [Int]
myReverse' l = rev l []
    where
      rev :: [Int] -> [Int] -> [Int]
      rev []     l' = l'
      rev (x:xs) l' = rev xs (x:l')

-- | Concatenate a list of lists.
myConcat :: [[Int]] -> [Int]
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
myProduct :: [Int] -> Int
myProduct (x:xs) = x * myProduct(xs)
myProduct []     = 1

-- | take n, applied to a list xs, returns the prefix of xs of length
-- n, or xs itself if n > length xs.
myTake :: Int -> [Int] -> [Int]
myTake n _      | n <= 0 = []
myTake _ []              = []
myTake n (x:xs)          = x : myTake (n-1) xs

-- | drop n xs returns the suffix of xs after the first n elements, or
-- [] if n > length xs.
myDrop :: Int -> [Int] -> [Int]
myDrop n l      | n <= 0 = l
myDrop _ []              = []
myDrop n (_:xs)          = myDrop (n-1) xs

-- | The insert function takes an element and a list and inserts the
-- element into the list at the last position where it is still less
-- than or equal to the next element.
myInsert :: Int -> [Int] -> [Int]
myInsert e (x:xs) | e > x     = x : myInsert e xs
                  | otherwise = e : x : xs
myInsert e []                 = [e]

-- | The sort function.
mySort :: [Int] -> [Int]
mySort l =  srt l []
    where
      srt :: [Int] -> [Int] -> [Int]
      srt (x:xs) l' = srt xs (myInsert x l')
      srt []     l' = l'
