
import Data.List
import Data.Fixed
import IOUtils
import ASCIIHist

main = do [a, r0, m] <- sequence $ map prompt ["a>", "r0", "m"]
          let rnd    = r0 : [ (r * a) `mod'` m | r <- rnd]
              rnd'   = drop (m `div'` 2 + 1) rnd
              unique = takeWhile (/= head rnd') $ tail rnd'
              t      = length unique
              ap     = (length $ takeWhile (`notElem` unique) rnd) + t
              rands  = map (/m) unique
              µ      = sum rands / genericLength rands
              var    = (sum $ map ( \x -> (x - µ)^2 ) rands) / (genericLength rands)
              σ      = sqrt var

          putStrLn . unlines . map print' $ [("ap = ", fromIntegral ap), ("t = ", fromIntegral t), ("µ = ", µ), ("σ = ", σ)]
          putStr . unlines $ histogram rands 20
