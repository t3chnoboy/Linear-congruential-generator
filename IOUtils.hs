module IOUtils where

prettyPrint (msg, val) =  msg ++ show val

prompt msg = do
    putStrLn msg
    inp <- getLine
    return (read inp)
