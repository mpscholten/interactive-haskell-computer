-- Regression: a multi-line `deriving` clause — the `deriving` keyword on its
-- own line with the class list in parens on the FOLLOWING line(s) — must still
-- capture the derived classes.  http-types' StdMethod uses exactly this layout
-- (it also reaches it via the warp request path).  Before the fix the deriving
-- scanners in IHC.Scan (`scanSimpleDerivingsRaw` and `scanFunctorDerivingsRaw`,
-- via `scanClasses`) saw the newline right after `deriving`, hit their catch-all
-- and captured ZERO classes — so derived Enum/Bounded/Ix were silently dropped
-- and `minBound :: Color` fell back to the Int `Bounded` instance (printing the
-- Int minBound; `[minBound .. maxBound]` then heap-exhausts over the Int range).
data Color = Red | Green | Blue
    deriving
        ( Show
        , Eq
        , Ord
        , Enum
        , Bounded
        )

main :: IO ()
main = do
    print (minBound :: Color)
    print (maxBound :: Color)
    print (fromEnum Green)
