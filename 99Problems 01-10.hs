import Data.List
-- Problem 1 
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (_:xs) c = elementAt xs (c-1)

-- Problem 4
myLength (_:[]) = 1
myLength (_:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse xs =  myReverse' xs []
	where
		myReverse' [] reverse = reverse
		myReverse' (x:xs) reverse = myReverse' xs (x:reverse)

--Problem 6
isPalindrome xs = xs == myReverse xs

--Problem 7
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten a = flt' a []
	where
		flt' (Elem x) xs = x:xs
		flt' (List (x:ls)) xs = flt' x (flt' (List ls) xs)
		flt' (List []) xs = xs

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [x] = [x]
compress (x:y:xs) = if x == y 
						then compress (x:xs)
						else x : (compress (y:xs))

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first, rest) = span (==x) xs
					in (x:first) : pack rest
-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [ (length x, head x) | x <- group xs]
