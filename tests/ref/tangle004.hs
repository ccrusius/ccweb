{-# LINE 54 "tests/tangle.org" #-}
main :: IO ()
main = do
  putStrLn str
{-# LINE 72 "tests/tangle.org" #-}
  // The line pragma should still be a Haskell one
  std::cerr << "This is not C++" << std::endl;
{-# LINE 58 "tests/tangle.org" #-}
  putStrLn "(" >> 
{-# LINE 72 "tests/tangle.org" #-}
                  // The line pragma should still be a Haskell one
                  std::cerr << "This is not C++" << std::endl;
{-# LINE 58 "tests/tangle.org" #-}
                                                               >> putStrLn ")"
  putStrLn str
{-# LINE 63 "tests/tangle.org" #-}
str :: String
str = "Hello, world!"
--!! tests/tangle.org:78
-- Line pragma above should start with --!!
--!! tests/tangle.org:84
-- And the pragma above should start with --!! too.
{-# LINE 89 "tests/tangle.org" #-}
-- There should be a regular line pragma above
{-# LINE 67 "tests/tangle.org" #-}
-- There should be no regular line pragma above pointing
-- to this source block.
{-# LINE 67 "tests/tangle.org" #-}
