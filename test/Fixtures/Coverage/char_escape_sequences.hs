-- Char escape sequences
main = do
    print '\n'
    print '\t'
    print '\\'
    print '\"'
    print '\''
    print '\0'
    -- numeric escapes
    print (fromEnum '\xff')
    print (fromEnum '\x41')
