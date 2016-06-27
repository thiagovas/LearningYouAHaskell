-- http://br.spoj.com/problems/PAPRIMAS/
-- By Thiago Silva (thiagovas.com)
import System.IO (isEOF)
import Data.Char


convert :: Char -> Int -> Int
convert '_' i = i
convert c i = case isLower c of
                True -> (ord c) - (ord 'a') + 1 + i
                False -> (ord c) - (ord 'A') + 27 + i


isPrime :: Int -> Int -> Bool
isPrime 1 _ = True
isPrime 2 _ = True
isPrime n 2 = (n `mod` 2) == 1
isPrime n d = case (n `mod` d) of
                0 -> False
                _ -> isPrime n (d-1)




solve :: String -> [Char]
solve s = case (isPrime t (t-1)) of
            True -> "It is a prime word."
            False -> "It is not a prime word."
          where
            t = foldr convert 0 s


loop = do done <- isEOF
          if done
              then return ()
              else do inp <- getLine
                      putStrLn (solve inp)
                      loop

main = loop
