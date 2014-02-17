import Data.List

main = do
    putStrLn "r0>"
    r0String <- getLine
    putStrLn "a>"
    aString <- getLine
    putStrLn "m>"
    mString <- getLine

    let
      r0     = read r0String :: Int
      a      = read aString :: Int
      m      = read mString :: Int

      rnd    = r0 : [ (r * a) `mod` m | r <- rnd]
      unique = nub $ take 15000 rnd
      minR   = minimum unique
      maxR   = maximum unique
      period = length unique
      points = map (\x -> fromIntegral x/ fromIntegral m) unique
      mx     = sum points / genericLength points
      dx     = (sum $ map (\x -> (x - mx)^2) points) / (genericLength points)
      u      = sqrt dx

    print minR
    print maxR
    print period
    print $ take 5 rnd
    print $ take 5 points
    print mx
    print dx
    print u
