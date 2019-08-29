module Calendar where
import Prelude (Int, Char, String, Show(..), (++))

data GTime = GTime Week Date Time

data Date = Date Year Month Day

newtype Year = Year Int

data Month = January    | February   | March     | April    |
             May        | June       | September | October  |
             July       | August     | November  | December

newtype Day = Day Int

data Week = Monday   | Tuesday | Wednesday | 
            Thursday | Friday  | Saturday  |
            Sunday

data Time = Time Hour Minute Second

newtype Hour   = Hour   Int
newtype Minute = Minute Int
newtype Second = Second Int

instance Show Month where 
    show January = "January"
    show February = "February" 
    show March = "March"
    show April = "April"
    show May = "May"
    show June = "June"
    show September = "September"
    show October = "October"
    show July = "July"
    show August = "August"
    show November = "November"
    show December = "December"

instance Show Week where 
    show Monday = "Monday" 
    show Tuesday = "Tuesday"
    show Wednesday = "Wednesday"
    show Thursday = "Thursday"
    show Friday = "Friday"
    show Saturday = "Saturday"
    show Sunday = "Sunday"

instance Show Time where
    show (Time h m s) = show h ++ ":" ++ show m ++ ":" ++ show s

instance Show Hour where
    show (Hour h) = addZero (show h)
instance Show Minute where
    show (Minute m) = addZero (show m)
instance Show Second where
    show (Second s) = addZero (show s)

addZero :: String -> String 
addZero [a] = ['0', a] 
addZero as = as

instance Show GTime where
    show (GTime w d t) = show w ++ ", " ++ show d ++ " " ++ show t

instance Show Date where
    show (Date y m d) = show d ++ " " ++ show m ++ " " ++ show y

instance Show Day where
    show (Day d) = show d

instance Show Year where
    show (Year y) = show y