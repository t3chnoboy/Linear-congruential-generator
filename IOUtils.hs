module IOUtils where

print' (msg, val) =  msg ++ show val

prompt msg = do
    putStrLn msg
    inp <- getLine
    return (read inp)
