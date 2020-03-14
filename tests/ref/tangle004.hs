{-# LINE 54 "tests/tangle.org" #-}
main :: IO ()
main = do
  putStrLn str
{-# LINE 70 "tests/tangle.org" #-}
  // The line pragma should still be a Haskell one
  std::cerr << "This is not C++" << std::endl;
{-# LINE 58 "tests/tangle.org" #-}
  putStrLn "(" >> 
{-# LINE 70 "tests/tangle.org" #-}
                  // The line pragma should still be a Haskell one
                  std::cerr << "This is not C++" << std::endl;
{-# LINE 58 "tests/tangle.org" #-}
                                                               >> putStrLn ")"
  putStrLn str
{-# LINE 63 "tests/tangle.org" #-}
str :: String
str = "Hello, world!"
--!! tests/tangle.org:76
-- Line pragma avove should start with --!!
-- There should be no line pragma above
{-# LINE 86 "tests/tangle.org" #-}
-- There should be a line pragma above
{-# LINE 91 "tests/tangle.org" #-}
-- There should be a line pragma above
{-# LINE 65 "tests/tangle.org" #-}
