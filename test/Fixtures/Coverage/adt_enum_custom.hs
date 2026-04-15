data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

dayName Mon = "Monday"
dayName Tue = "Tuesday"
dayName Wed = "Wednesday"
dayName Thu = "Thursday"
dayName Fri = "Friday"
dayName Sat = "Saturday"
dayName Sun = "Sunday"

main = do
    putStrLn (dayName Wed)
    print (isWeekend Sat)
    print (isWeekend Mon)
