import Data.Char


-- Excercise 1

ex1 x = 2*x^2 + 3*x - 5


-- Exercise 2
abc = "abcdefghijklmnopqrstuvwxyz"

code :: Char -> Char
code x          | ord(x) > 96 && ord(x) < 123   = chr(97 + mod (ord(x)-97+3) 26)
                | ord(x) > 64 && ord(x) < 91    = chr(65 + mod (ord(x)-62+3) 26)
                | otherwise                     = x

code' x = coden2 3 x

coden :: Int -> Char -> Char
coden n x       | ord(x) > 96 && ord(x) < 123   = chr(97 + mod (ord(x)-97+n) 26)
                | ord(x) > 64 && ord(x) < 91    = chr(65 + mod (ord(x)-65+n) 26)
                | otherwise                     = x

-- Variant met 25 letters!
-- Omdat het.. Omdat het prachtig is! - Wiebes, 2017
coden2 :: Int -> Char -> Char
coden2 n x      | ord(x) > 96 && ord(x) < 122   = chr(97 + mod (ord(x)-97+n) 25)
                | ord(x) > 64 && ord(x) < 90    = chr(65 + mod (ord(x)-65+n) 25)
                | otherwise                     = x

codestr :: [Char]
codestr = map code "hello"

codestr2 :: [Char]
codestr2 = map code "Tomorrow evening, 8 o\'clock in Amsterdam"


--Exercise 3
interest :: Floating a => a -> a -> a -> a
interest a r n        = (a*(1+r)**n)

amount :: (Floating a, Eq a) => a -> a -> a -> a
amount a r 0    = a
amount a r n    = amount (a*(1+r)) r (n-1)


-- Exercise 4
-- Nice test function, not part of the exercise. Can be extended to catch the errors of complex roots.
roots :: Double -> Double -> Double -> (Double, Double)
roots a b c = (root1 a b c, root2 a b c)

root1 ::  Double -> Double -> Double -> Double
root1 a b c     | disc a b c < 0        = error "negative discriminant"
                | otherwise             = (-b + sqrt(disc a b c)) / (2*a)

root2 ::  Double -> Double -> Double -> Double
root2 a b c     | disc a b c < 0        = error "negative discriminant"
                | otherwise             = (-b - sqrt(disc a b c)) / (2*a)

disc ::  Double -> Double -> Double -> Double
disc a b c      = (b*b)-4*a*c


-- Exercise 5
extrX :: Double -> Double -> Double -> Double
extrX a b c = -b/(2*a)

extrY :: Double -> Double -> Double -> Double
extrY a b c = a*(exX)^2 + b*(exX) + c
    where exX = extrX a b c


-- Exercise 6
mylength :: [a] -> Int
mylength []     = 0
mylength (x:xs) = 1 + mylength xs

mysum :: [Int] -> Int
mysum []        = 0
mysum (x:xs)    = x + mysum xs

myreverse :: [a] -> [a]
myreverse []            = []
myreverse (x:xs)        = myreverse xs ++ [x]

mytake :: Int -> [a] -> [a]
mytake _ []     = []
mytake 0 xs     = []
mytake n (x:xs) = [x] ++ mytake (n-1) xs

myelem :: Eq a => a -> [a] -> Bool
myelem _ []     = False
myelem a (x:xs) | x == a        = True
                | otherwise     = myelem a xs

myconcat :: [[a]] -> [a]
myconcat []     = []
myconcat (x:xs) = x ++ myconcat xs

mymaximum :: [Int] -> Int
mymaximum []            = error "cannot determine maximum of empty list"
mymaximum [x]           = x
mymaximum (x:xs)        | x <= mymaximum(xs)    = mymaximum(xs)
                        | otherwise             = x

myzip :: [a] -> [b] -> [(a, b)]
myzip (x:xs) []         = []
myzip [] (y:ys)         = []
myzip (x:xs) (y:ys)     = (x,y) : myzip xs ys


-- Exercise 7
r :: Num a => a -> a -> [a]
r a d   = a : r (a+d) d

r1 :: Num a => a -> a -> Int -> a
r1 a d 0        = a
r1 a d n        = (r a d)!!n

--total1 is better
total :: (Eq a, Num a) => Int -> Int -> a -> Int -> Int
total a d i 0   = 0
total a d 0 j   = mysum (mytake j (r a d))
total a d i j   = total (a+d) d (i-1) (j-1)

total1 :: (Num a) => a -> a -> Int -> Int -> a
total1 a d i j  | i == j        = (r1 a d j)
                | i > j         = error "i must be smaller than j"
                | otherwise     = (r1 a d i) + total1 a d (i+1) j


-- Exercise 8
allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual [x]    = True
allEqual (x:xs) | x == head xs  = allEqual xs
                | otherwise     = False

isAS :: (Num a, Eq a) => [a] -> Bool
isAS xss@(x1:x2:xs) = allEqual $ zipWith (-) (r x1 (x2-x1)) xss

isAS' []    = True
isAS' [_]   = True
isAS' [_,_] = True
isAS' xss@(x1:x2:x3:xs) | x2-x1 == x3-x2    = isAS' (x2:x3:xs)
                        | otherwise         = False


-- Exercise 9
isMatrix :: [[a]] -> Bool
isMatrix a = allEqual(map mylength a)

rowTotals :: [[Int]] -> [Int]
rowTotals a = map mysum a

transposeMatrix :: (Eq a) => [[a]] -> [[a]]
transposeMatrix a       | head a == []   = []
                        | otherwise      = map head a : transposeMatrix (map tail a)

columnTotals :: [[Int]] -> [Int]
columnTotals a          | mylength (head a) == 0        = []
                        | otherwise                     = mysum(map head a) : columnTotals (map tail a)

columnTotals' :: [[Int]] -> [Int]
columnTotals' = rowTotals . transposeMatrix
