echo :: IO ()
echo = getChar >>= \ c -> putChar c

getUpper :: IO Char
getUpper
  = getChar >>= \ c -> return (toUpper c)
