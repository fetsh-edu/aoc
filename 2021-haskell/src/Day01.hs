module Day01 (solve1, solve2) where
 
import Relude (Text)
import qualified Data.Text as Text
  
solve1 :: String -> Text
solve1 input =
    Text.reverse (Text.pack input)
    
solve2 :: String -> Text
solve2 input =
    "output2"