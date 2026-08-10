import CallerA (increment)
import CallerB (observe)

main :: IO Int
main = do
    increment
    observe
