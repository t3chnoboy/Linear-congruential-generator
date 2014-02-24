{-Taken from http://hackage.haskell.org/package/monad-mersenne-random-0.1/docs/Control-Monad-Mersenne-Random.html-}

module ASCIIHist where
import Text.Printf


histogram :: [Double] -> Int -> [String]
histogram _ 0 = []
histogram xs bins =
   let xmin = minimum xs
       xmax = maximum xs
       bsize = (xmax - xmin) / fromIntegral bins
       bs = take bins $ zip [xmin,xmin+bsize..] [xmin+bsize,xmin+2*bsize..]
       counts :: [Int]
       counts = let cs = map count bs
                in  init cs ++ [last cs + length (filter (==xmax) xs)]
   in  map (format (maximum counts)) $ zip bs counts
 where
   toD :: (Real b) => b -> Double
   toD = fromRational . toRational
   count (xmin, xmax) = length $ filter (\x -> x >= xmin && x < xmax) xs
   format :: Int -> ((Double,Double), Int) -> String
   format maxc ((lo,hi), c) = 
       let cscale = 50.0 / toD maxc
           hashes = replicate (round $ toD c * cscale) '#'
           label  = let los = printf "%.2g" lo
                        his = printf "%.2g" hi
                        l   = los ++ " .. " ++ his
                        pad = replicate (20 - length l) ' '
                    in  pad ++ l
       in  label ++ ": " ++ hashes
