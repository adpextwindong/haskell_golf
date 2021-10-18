foo :: IO ()
foo = getLine >>= (\name -> print ("Hello " ++ name))

bar :: IO ()
bar = do
    name <- getLine
    print ("Hello " ++ name)
