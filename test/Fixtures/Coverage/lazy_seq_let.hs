-- seq forces the first argument to WHNF before returning the second
main = do
    let x = error "forced!" `seq` 42
    let y = 1 + 1
    print y
    -- x would error if forced, but we don't force it
    let z = seq (1 + 1) "hello"
    putStrLn z
