import Distribution.Simple.Utils
-- module Main

d1 :: String -> IO ()
d1 a = do 
  let test = sum . map read . words $ a
  print test

