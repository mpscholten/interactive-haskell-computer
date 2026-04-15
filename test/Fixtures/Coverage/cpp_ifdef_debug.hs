{-# LANGUAGE CPP #-}

#ifdef DEBUG
debugMsg = "debug build"
#else
debugMsg = "release build"
#endif

main = putStrLn debugMsg
