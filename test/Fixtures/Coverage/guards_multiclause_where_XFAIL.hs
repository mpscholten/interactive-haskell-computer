bmi weight height
    | bmiVal <= 18.5 = "underweight"
    | bmiVal <= 25.0 = "normal"
    | bmiVal <= 30.0 = "overweight"
    | True           = "obese"
  where
    bmiVal = weight / (height * height)

main = do
    putStrLn (bmi 50.0 1.7)
    putStrLn (bmi 70.0 1.75)
    putStrLn (bmi 90.0 1.7)
