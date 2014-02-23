import Data.List
import Data.Fixed

prettyPrint (msg, val) =  msg ++ show val

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

      rnd    = r0 : [ (r * a) `mod'` m | r <- rnd]
      unique = takeWhile (/= head rnd) $ tail rnd
      t      = length  unique
      points = map (/m) unique
      µ      = sum points / genericLength points
      var    = (sum $ map ( \x -> (x - µ)^2 ) points) / (genericLength points)
      σ      = sqrt var

    putStrLn . unlines. map prettyPrint $ [("T = ", fromIntegral t), ("µ = ", µ), ("σ = ", σ)]
