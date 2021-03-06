import Data.List
import Data.Fixed
import IOUtils
import ASCIIHist

main = do [a, r0, m] <- sequence $ map prompt ["a>", "r0", "m"]
          let rnd    = r0 : [ (r * a) `mod'` m | r <- rnd]
              rnd'   = tail rnd
              unique = takeWhile (/= head (tail rnd)) $ tail $ tail rnd
              t      = length  unique
              points = map (/m) unique
              µ      = sum points / genericLength points
              var    = (sum $ map ( \x -> (x - µ)^2 ) points) / (genericLength points)
              σ      = sqrt var

          putStrLn . unlines. map print' $ [("T = ", fromIntegral t), ("µ = ", µ), ("σ = ", σ)]
          putStr . unlines $ histogram points 20
