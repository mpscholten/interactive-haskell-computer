-- OverloadedRecordDot chained: x.a.b two-level access.
data City = City { cityName :: String }
data Address = Address { city :: City }
data Person = Person { address :: Address }

main = do
    let c = City { cityName = "Berlin" }
    let a = Address { city = c }
    let p = Person { address = a }
    putStrLn p.address.city.cityName
