r0 = 107839
a = 18221
m = 510300

rnd = r0 : [ (r * a) `mod` m | r <- rnd]

main = do
    print $ take 10 $ rnd
