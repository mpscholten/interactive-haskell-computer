x ! y = x + y

main :: IO ()
main = do
    let idxhdr = 10
        expect = idxhdr ! 32
        handle100Continue = expect + 1
    print handle100Continue
