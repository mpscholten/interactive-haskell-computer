{-# LANGUAGE TypeApplications #-}
-- Visible type application inside do (Warp `return @Payload`,
-- megaparsec `pure @Parser` / `parse @Void @Text`).
main = do
    x <- return @Int 42
    print x
    y <- pure @IO (1 :: Int)
    print y
    print =<< return @Int 7
