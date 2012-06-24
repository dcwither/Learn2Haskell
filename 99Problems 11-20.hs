import Data.List

-- Problem 11
data ListItem a = Single a | Multiple Int a
	deriving (Show)
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified xs = [if (length x) == 1
						then Single (head x)
						else Multiple (length x) (head x)
					| x <- group xs]
-- Alternatively
-- encodeModified xs = [y | x <- pack xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]
 
--Problem 12

decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = (decodePortion x) ++ (decodeModified xs)

decodePortion :: Eq a => ListItem a -> [a]
decodePortion (Single x) = [x]
decodePortion (Multiple len x) = replicate len x


