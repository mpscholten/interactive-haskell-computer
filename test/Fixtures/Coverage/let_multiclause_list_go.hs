-- Multi-clause function bound in a layout @let@ (not @where@ / do-let).
-- bytestring's packUptoLenChars uses this shape:
--
--   packUptoLenChars len cs0 =
--     unsafeCreateFpUptoN' len $ \p0 ->
--       let go !p []     = ...
--           go !p (c:cs) = ...
--       in go p0 cs0
--
-- Before the fix, only the last equation survived as a wrapParams
-- lambda on @(c:cs)@, so consuming the empty-list tail raised
-- "Non-exhaustive patterns in let: PCon \":\" …" — which blocked
-- Lazy.Char8.pack and therefore responseLBS string bodies.

packUpto :: Int -> String -> (Int, String)
packUpto n cs0 =
  let go !p []              = (p, [])
      go !p cs | p == n     = (p, cs)
      go !p (c:cs)          = go (p + 1) cs
  in go 0 cs0

myLength :: [a] -> Int
myLength xs =
  let go _ []     = 0
      go i (_:ys) = 1 + go (i + 1) ys
  in go 0 xs

main :: IO ()
main = do
    print (myLength "hi")
    print (myLength "")
    print (packUpto 32 "hello")
    print (packUpto 3 "hello")
    print (packUpto 32 "")
