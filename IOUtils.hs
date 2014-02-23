module IOUtils where

prettyPrint (msg, val) =  msg ++ show val

promptInt msg = do
    putStrLn msg
    inp <- getLine
    return (read inp)
