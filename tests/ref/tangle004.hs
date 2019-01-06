{-# LINE 54 "tests/tangle.org" #-}
main :: IO ()
main = do
  putStrLn str
{-# LINE 69 "tests/tangle.org" #-}
  putStrLn str
{-# LINE 58 "tests/tangle.org" #-}
  putStrLn "(" >> 
{-# LINE 69 "tests/tangle.org" #-}
                  putStrLn str
{-# LINE 58 "tests/tangle.org" #-}
                               >> putStrLn ")"
  putStrLn str
{-# LINE 63 "tests/tangle.org" #-}
str :: String
str = "Hello, world!"
