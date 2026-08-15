-- HSX leftover after unused parseHsx CAF is GREEN: forcing quoteExp
-- on the source-loaded `hsx` QuasiQuoter (a PAP of quoteHsxExpression)
-- must not hang.  Do not import the selector — EQuasiQuote / record-dot
-- resolve it via `$fldProj$quoteExp` and the TH.Quote field env.
-- Do not run the Q action (parseHsx apply is a separate leftover).
import IHP.HSX.QQ (hsx)

main :: IO ()
main = quoteExp hsx `seq` putStrLn "selector"
