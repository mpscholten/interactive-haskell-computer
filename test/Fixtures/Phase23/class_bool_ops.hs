-- Boolean operations return proper Bool constructors.
main = do
    print (True && True)
    print (True && False)
    print (False || True)
    print (False || False)
    print (not True)
    print (not False)
