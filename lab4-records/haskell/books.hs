import Text.Printf ( printf )
import Data.Time (Year)

-- this defines a record
-- this is a union type and its fields
data Information
    = Book {
        title :: String,
        author :: String,
        publisher :: String,
        year :: Year
    }
    | --denoted by pipe symbol
    Newspaper {
        title :: String,
        news_publisher :: String,
        year :: Year
    }
    deriving(Eq)

-- defining the methods required for the record
-- such as showing the record variants
instance (Show Information) where
    show (Book title author publisher year) =
        printf "%s -author-> %s, %s, %i" title author publisher year
    show (Newspaper title news_publisher year) =
        printf "News: %s written by %s" title news_publisher

-- definition of records
-- such as 2 newspapers and 1 book.
aljazeera = 
    Newspaper
    {
        title = "Tayib Erdogan attempts to win the 2023 Turkish Election",
        news_publisher = "Al-Jazeera",
        year = 2023
    }

aljazeera1 = 
    Newspaper
    {
        title = "Purposeful Assassination of Lebanon President Rafik Hariri.",
        news_publisher = "Al-Jazeera",
        year = 2005
    }

aljazeera2 = 
    Newspaper
    {
        title = "Execution of Saddam Hussain",
        news_publisher = "Al-Jazeera",
        year = 2005
    }

book = 
    Book
    {
        title = "Matilda",
        author = "Roald Dahl",
        publisher = "Childs Book",
        year = 1980
    }


main = 
    do
        putStrLn $ show test0
        putStrLn $ show test1
        putStrLn $ show aljazeera
        putStrLn $ show aljazeera1


-- a formation of pattern matching
sortPublicationsByYear :: (Information, Information) -> (Information, Information)
(test0, test1) = sortPublicationsByYear(book, aljazeera1)

sortPublicationsByYear (item1, item2) =
    if year item1 < year item2
        then (item1, item2);
        else (item2, item1)

-- these are variants of pattern matching
readNews (x,_) = printf $ title x ++ "\n"

readNews1(_,y) = printf $ title y ++ "\n"


readBothInfo (x :: Information , y :: Information) = 
    do
        putStrLn $ title x ++ "\n"
        putStrLn $ title y ++ "\n"

-- this sorts the year based off publcation which is by year
sortByOldestInfo (x:: Information, y :: Information) =
    do
        if year x < year y 
            then 
                putStrLn $ title x ++ "\nPublished at: " ++ show(year x) ++ "\n" ++ title y ++ "\nPublished at: " ++  show (year y)
            else
                sortByOldestInfo(y, x)