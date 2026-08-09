import qualified OwnerAlpha as A
import qualified OwnerBeta as B

main :: IO ()
main = do
    putStrLn A.renderAlpha
    putStrLn B.renderBeta
