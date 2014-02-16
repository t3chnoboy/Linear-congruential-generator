import Data.List
import Data.Fixed

r0 = 107839
a  = 18221
m  = 510300

rnd = r0 : [ (r * a) `mod'` m | r <- rnd]

unique = nub $ take 5000 rnd
minR = minimum unique
maxR = maximum unique
period = length unique
points = map (/m) unique
mx = sum points / genericLength points
dx = (sum $ map (\x -> (x - mx)^2) points) / (genericLength points)

main = do
    print minR
    print maxR
    print period
    print $ take 5 rnd
    print $ take 5 points
    print mx
    print dx
