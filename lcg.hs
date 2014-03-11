import Data.List
import Data.Fixed
import IOUtils
import ASCIIHist

main = do [a, r0, m] <- sequence [prompt "a>", prompt "r0", prompt "m"]
          let
            rnd    = r0 : [ (r * a) `mod'` m | r <- rnd]
            rnd'   = tail rnd
            unique = takeWhile (/= head rnd') $ tail rnd'
            t      = length  unique
            points = map (/m) unique
            µ      = sum points / genericLength points
            var    = (sum $ map ( \x -> (x - µ)^2 ) points) / (genericLength points)
            σ      = sqrt var

          putStrLn . unlines. map prettyPrint $ [("T = ", fromIntegral t), ("µ = ", µ), ("σ = ", σ)]
          putStr . unlines $ histogram points 20
