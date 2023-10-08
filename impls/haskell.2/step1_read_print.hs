module Main where
import System.IO

read' :: String -> String
read' x = x

eval' :: a -> a
eval' x = x

print' :: String -> IO ()
print' x =
    putStrLn x

rep :: IO ()
rep =
  do
    putStr "user> "
    hFlush stdout
    line <- getLine
    (print' . eval' . read' ) line
    rep

main :: IO ()
main = rep

