import Data.List
import Data.Fixed
import IOUtils

main = do
    a <- promptInt "a>"
    r0 <- promptInt "r0"
    m <- promptInt "m"
    let
      rnd    = r0 : [ (r * a) `mod'` m | r <- rnd]
      unique = takeWhile (/= head rnd) $ tail rnd
      t      = length  unique
      points = map (/m) unique
      µ      = sum points / genericLength points
      var    = (sum $ map ( \x -> (x - µ)^2 ) points) / (genericLength points)
      σ      = sqrt var

    putStrLn . unlines. map prettyPrint $ [("T = ", fromIntegral t), ("µ = ", µ), ("σ = ", σ)]
