-- Eq on lists: structural equality via VCon dispatch.
main = do
    print ([1,2,3] == [1,2,3])
    print ([1,2,3] == [1,2,4])
    print ([] == [])
    print ("hello" == "hello")
    print ("hello" == "world")
