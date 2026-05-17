-- Eq on the canonical parametric stdlib types Maybe / Either — their
-- Eq instances live in base source and reach the registry via the
-- normal instance path; this confirms the cascade fix didn't break
-- their per-FV registration.
main :: IO ()
main = do
    print ((Just (1 :: Int)) == Just 1)
    print ((Just (1 :: Int)) == Just 2)
    print ((Nothing :: Maybe Int) == Nothing)
    print ((Just (1 :: Int)) /= Nothing)
    print ((Left 'a' :: Either Char Int) == Left 'a')
    print ((Right 5 :: Either Char Int) == Right 5)
    print ((Left 'a' :: Either Char Int) == Right 5)
