-- Char literals for structural characters that must not be lexed as TkTick.
-- Seen in: hasql TextBuilder.char '[', IHP HSX char '{'/'}'.
main = do
    print '['
    print ']'
    print '{'
    print '}'
    print '('
    print ')'
    print (['[', ']', '{', '}', '(', ')'] == "[]{}()")
    print ('[' == '[')
    print (']' /= '[')
