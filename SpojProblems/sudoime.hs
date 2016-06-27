-- http://br.spoj.com/problems/SUDOIME/
-- By Thiago Silva (thiagovas.com)
import System.IO (isEOF)
import Data.Char


-- Function that reads an integer from standard input.
getInt :: IO Int
getInt = do str <- getLine
            return (read str)


-- Function that tells if the puzzle is solved or not
-- It returns the String "YES" if the puzzled was solved correctly
-- "NO" otherwise 
solve puzzle = return "ab"


-- Function that reads the puzzle
readPuzzle = return ()



loopInstances 0 _ = return ()
loopInstances nInstances index = do puzzle <- readPuzzle
                                    putStrLn ("Instancia " + index)
                                    putStrLn (solve puzzle)
                                    putStrLn ""



loop = do done <- isEOF
          if done
              then return()
              else do inp <- getInt
                      loopInstances inp 1
                      loop

main = loop

