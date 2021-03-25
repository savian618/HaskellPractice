
counting :: (Integral n, ls []) -> Integral
counting n ls =
    if null ls then 0
    else if n == head ls then 1  + counting n (tail ls)
    else counting n (tail ls)

counting_tuples :: (ls []) -> [(Char, Integral)]
counting_tuples n
    | null n = True
    | otherwise [(a, b)| a <- n, b <- [counting a n]]

descending :: (Ord a) => [a] -> Bool
descending ls
    | null ls                   = True
    | length ls == 1            = True
    | head ls >= head (tail ls) = True
    | head ls < head (tail ls)  = False
    | otherwise descending tail

divs :: (Integral n) => n -> [Integral]
divs n
    | n >= 1      = [x | x <- [1..(n)], n `mod` x == 0]
    | otherwise   = []


isPrime :: (Integral n) => n -> Bool
isPrime n 
    | n < 2                                = False 
    | length([ x | x <- [2..n - 1], mod n x == 0]) = True
    | otherwise                            = False

fakePrimes ::[Integral] -> [Integral]
fakePrimes = [x | x <-[2..], x `mod` 2 /= 0, x `mod` 3 /= 0, x `mod` 5 /= 0, x `mod` 7 /= 0, x `mod` 9 /= 0, not (isPrime x)]
	

