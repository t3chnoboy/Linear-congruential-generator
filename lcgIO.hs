import Data.List

main = do
    putStrLn "r0>"
    r0String <- getLine
    putStrLn "a>"
    aString  <- getLine
    putStrLn "m>"
    mString  <- getLine

    let
      r0     = read r0String
      a      = read aString
      m      = read mString

      rnd    = r0 : [ (r * a) `mod` m | r <- rnd]
      unique = takeWhile (/= head rnd) $ tail rnd
      minR   = minimum unique
      maxR   = maximum unique
      period = length  unique
      points = map (\x -> fromIntegral x / fromIntegral m) unique
      μ      = sum points / genericLength points
      var    = (sum $ map ( \x -> (x - μ)^2 ) points) / (genericLength points)
      σ      = sqrt var

    print minR
    print maxR
    print period
    print unique
    print $ take 10 points
    print μ
    print var
    print σ
