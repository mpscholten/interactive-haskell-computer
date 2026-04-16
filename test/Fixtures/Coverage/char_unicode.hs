-- Unicode character literals
main = do
    print 'λ'
    print 'α'
    print 'π'
    putStrLn ['H','e','l','l','o']
    print (fromEnum 'A')
    print (toEnum 65 :: Char)
