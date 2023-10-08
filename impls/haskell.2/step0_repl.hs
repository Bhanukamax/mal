module Main where
import System.IO

rep :: IO ()
rep =
  do
    putStr "user> "
    hFlush stdout
    line <- getLine
    putStrLn line
    rep

main :: IO ()
main = rep

