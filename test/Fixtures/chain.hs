-- Chain: main -> a -> b -> c. Each binding calls the next.
main = a
a = b + 1
b = c * 2
c = 20
-- Answer: 20 * 2 + 1 = 41
