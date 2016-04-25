
-- Defining a zip function
f a [] = []
f [] b = []
f (a:x) (b:y) = (a, b):(f x y)


-- Defining a zip function with the zipWith
z a b = zipWith (\x y -> (x, y)) a b


-- Defining a zipWith function
tZipWith g (a:x) (b:y) = (g a b):(tZipWith g x y)
tZipWith g _ _ = []


-- Defining another function with the same type as zipWith
dZipWith g (a:x) (b:y) = (g a b):((g a b):dZipWith g x y)
dZipWith g _ _ = []
