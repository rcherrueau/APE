mySub :: Int -> Int -> Int
mySub x y = x-y

mySub' :: Int -> Int -> Int
mySub' x y = (-) x y

mySub'' :: Int -> Int -> Int
mySub'' x y = x `mySub` y

myNeg :: Int -> Int
myNeg x = mySub 0 x

myNeg' :: Int -> Int
myNeg' = mySub 0

myHead :: [Int] -> Int
myHead (x:_) = x

myTail :: [Int] -> [Int]
myTail (_:xs) = xs

myAppend :: [Int] -> [Int] -> [Int]
myAppend (x:xs) ys = x : myAppend xs ys
myAppend []     ys = ys

myAppend' :: [Int] -> [Int] -> [Int]
myAppend' xs ys | not (null xs) = head xs : myAppend' (tail xs) ys
                | null xs       = ys
--                | otherwise = ys

myAppend'' :: [Int] -> [Int] -> [Int]
myAppend'' (x:xs) ys = x : suite
    where
      suite :: [Int]
      suite = myAppend'' xs ys
myAppend'' []     ys = ys

myAppend''' :: [Int] -> [Int] -> [Int]
myAppend''' (x:xs) ys = let suite = myAppend''' xs ys
                        in x : suite
myAppend''' []    ys = ys

myAppend'''' :: [Int] -> [Int] -> [Int]
myAppend'''' xs ys = myAppendAux xs
    where
      myAppendAux :: [Int] -> [Int]
      myAppendAux (x:xs) = x : myAppendAux xs
      myAppendAux []     = ys

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

-- pas d'element neutre pour max et min !

-- | take n, applied to a list xs, returns the prefix of xs of length
-- n, or xs itself if n > length xs.
myTake :: Int -> [Int] -> [Int]
myTake n _ | n <= 0 = []
myTake _ []         = []
myTake n (x:xs)     = x : myTake (n-1) xs

-- | drop n xs returns the suffix of xs after the first n elements, or
-- [] if n > length xs.
myDrop :: Int -> [Int] -> [Int]
myDrop n l | n <= 0 = l
myDrop _ []         = []
myDrop n (_:xs)     = myDrop (n-1) xs

myBangBang :: [Int] -> Int -> Int
myBangBang = undefined

-- | The insert function takes an element and a list and inserts the
-- element into the list at the last position where it is still less
-- than or equal to the next element.
myInsert :: Int -> [Int] -> [Int]
myInsert e (x:xs) | e > x     = x : myInsert e xs
                  | otherwise = e : x : xs
myInsert e []             = [e]

-- | The sort function implements a stable sorting algorithm.
mySort :: [Int] -> [Int]
mySort l =  mySort' l []
    where
      mySort' (x:xs) l' = mySort' xs (myInsert x l')
      mySort' [] l' = l'
