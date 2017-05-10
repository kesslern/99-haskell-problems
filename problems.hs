------------------
--- Problem 01 ---
------------------
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast2 :: [a] -> a
myLast2 x = last x

myLast3 :: [a] -> a
myLast3 xs = case xs of [] -> error "Empty list"
                        [x] -> x
                        (x:xs) -> myLast3 xs

myLast4 :: [a] -> a
myLast4 x = thelast x
 where theLast [] = error "Empty list"
       thelast [x] = x
       thelast (x:xs) = myLast4 xs

myLast5 :: [a] -> a
myLast5 x = head (reverse x)

------------------
--- Problem 02 ---
------------------
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [x] = error "Single element list"
myButLast (x:y:ys) = case ys of [] -> x
                                [_] -> y
                                ys -> myButLast ys

------------------
--- Problem 03 ---
------------------
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

------------------
--- Problem 04 ---
------------------
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

------------------
--- Problem 06 ---
------------------
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = case (x == (last xs)) of True -> isPalindrome (take ((length xs)-1) xs)
                                               False -> False

isPalindrome2 :: (Eq a) => [a] -> Bool
isPalindrome2 [] = True
isPalindrome2 [x] = True
isPalindrome2 (x:xs) = if x == (last xs)
                         then isPalindrome (take ((length xs)-1) xs)
                         else False

------------------
--- Problem 07 ---
------------------
data NestedList a = Elem a | List [NestedList a]
myFlatten :: [NestedList a] -> [a]
myFlatten [] = []
myFlatten (Elem x:xs) = x : myFlatten xs
myFlatten (List x:xs) = (myFlatten x) ++ myFlatten xs

------------------
--- Problem 08 ---
------------------
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if (x == (head xs))
                    then compress xs
                    else x : compress xs

------------------
--- Problem 09 ---
------------------
countIdentical :: (Eq a) => [a] -> Int
countIdentical [] = 0
countIdentical [x] = 1
countIdentical (x:xs) = if (x == (head xs))
                          then 1 + countIdentical xs
                          else 1

dropIdentical :: (Eq a) => [a] -> [a]
dropIdentical x = drop (countIdentical x) x

takeIdentical :: (Eq a) => [a] -> [a]
takeIdentical x = take (countIdentical x) x

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack n = takeIdentical n : pack (dropIdentical n)

pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
npack2 n = let first = (head n) in takeWhile (==first) n : pack2 (dropWhile (==first) n)

------------------
--- Problem 10 ---
------------------
encoder :: (Eq a) => [a] -> (Int, a)
encoder n = (length n, head n)

encode :: (Eq a) => [a] -> [(Int, a)]
encode n = map encoder (pack n)

encode2 :: (Eq a) => [a] -> [(Int, a)]
encode2 n = map (\x -> (length x, head x)) (pack n)

------------------
--- Problem 11
------------------
data SingleOrMultiple a = Multiple Int a | Single a deriving (Show)
encodeModified :: (Eq a) => [a] -> [SingleOrMultiple a]
encodeModified xs = map (\(n,x) -> if (n == 1) then Single x else Multiple n x) $ encode xs

------------------
--- Problem 12 ---
------------------
decodeModified :: (Eq a) => [SingleOrMultiple a] -> [a]
decodeModified xs = foldr1 (\(x,acc) -> acc) xs
