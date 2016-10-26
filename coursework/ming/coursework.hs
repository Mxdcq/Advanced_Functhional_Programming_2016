import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List.Split
import Data.List
import System.IO

main = do
    untagged <- readFile ("untagged.txt")
    let rawText = (lines untagged)

    countriesCont <- readFile ("./countries.txt")
    citiesCont <- readFile ("./cities.txt")
    personsCont <- readFile ("./persons.txt")
    let countries = (lines countriesCont)
    let cities = (lines citiesCont)
    let persons = (lines personsCont)

    let dateWords = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "Monday", "Tuesday", "Wednsday", "Thursday", "Friday", "Saturday", "Sunday"]

    let result = tagLines rawText countries cities persons dateWords []
    print rawText
    print "-------------------"

    print result

    writeFile "result.txt" $ unlines result

    tagged <- readFile ("tagged.txt")
    let taggedLines = (lines tagged)
    myTagged <- readFile ("result.txt")
    let myTaggedLines = (lines myTagged)

    let tagsFromTagged = getTags taggedLines []
    let tagsFromMyTagged = getTags myTaggedLines []

    let countFromTagged = countTagsFromLine tagsFromTagged
    let countFromMyTagged = countTagsFromMyLine tagsFromMyTagged

    print ("_______")
    print tagsFromTagged
    print ("*******")
    print tagsFromMyTagged
    print ("__should have been labelled in total_____")
    print countFromTagged
    print ("__correctly labelled in total_____")
    print countFromMyTagged

--rawText countries cities persons dateWords []
--Main function
tagLines :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String]
tagLines [] _ _ _ _ x = x
tagLines (x:xs) countries cities persons dateWords result =
    tagLines xs countries cities persons dateWords (result ++ [(asList (checkGazetteerList (checkForMoney(checkDateWords (iterTwoWordChunks (checkForOrganizations (splitOn " " x) []) countries cities persons []) dateWords []) []) countries cities persons []))])

--Check the contiguous two words.
iterTwoWordChunks :: [String] -> [String] -> [String] -> [String] -> [String] -> [String]
iterTwoWordChunks [] _ _ _ x = x
iterTwoWordChunks (x:xs) countries cities persons result
    | (null xs) == False && twoWordChunk `elem` countries = iterTwoWordChunks (tail xs) countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ twoWordChunk ++"</ENAMEX>")])
    | (null xs) == False && twoWordChunk `elem` cities = iterTwoWordChunks (tail xs) countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ twoWordChunk ++"</ENAMEX>")])
    | (null xs) == False && x `elem` persons && (head xs) `elem` persons = iterTwoWordChunks (tail xs) countries cities persons (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ twoWordChunk ++"</ENAMEX>")])
    | otherwise = iterTwoWordChunks (xs) countries cities persons (result ++ [x])
    where twoWordChunk = (x ++ " " ++ (head xs))

--Compare words with gazetteer files.
checkGazetteerList :: [String] -> [String] -> [String] -> [String] -> [String] -> [String]
checkGazetteerList [] _ _ _ x = x
checkGazetteerList (x:xs) countries cities persons result
    | x `elem` countries = checkGazetteerList xs countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ x ++"</ENAMEX>")])
    | x `elem` cities = checkGazetteerList xs countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ x ++"</ENAMEX>")])
    | x `elem` persons = checkGazetteerList xs countries cities persons (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ x ++"</ENAMEX>")])
    | (length x) > 1 && (init x) `elem` countries = checkGazetteerList xs countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ (init x) ++"</ENAMEX>" ++ [(last x)])])
    | (length x) > 1 && (init x) `elem` cities = checkGazetteerList xs countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ (init x) ++"</ENAMEX>" ++ [(last x)])])
    | (length x) > 1 && (init x) `elem` persons = checkGazetteerList xs countries cities persons (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ (init x) ++"</ENAMEX>" ++ [(last x)])])
    | (length x) > 2 && (init (init x)) `elem` countries = checkGazetteerList xs countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ (init (init x)) ++"</ENAMEX>" ++ [(last (init x))] ++ [(last x)])])
    | (length x) > 2 && (init (init x)) `elem` cities = checkGazetteerList xs countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ (init (init x)) ++"</ENAMEX>" ++ [(last (init x))] ++ [(last x)])])
    | (length x) > 2 && (init (init x)) `elem` persons = checkGazetteerList xs countries cities persons (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ (init (init x)) ++"</ENAMEX>" ++ [(last (init x))] ++ [(last x)])])
    | otherwise = checkGazetteerList xs countries cities persons (result ++ [x])

--Compare words with DateWords and some key words like "next", "last".
checkDateWords :: [String] -> [String] -> [String] -> [String]
checkDateWords [] _ x = x
checkDateWords (x:xs) dateWords result
    | x `elem` ["last", "next"] && (head xs) `elem` dateWords = checkDateWords (tail xs) dateWords (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++ " " ++ (head xs) ++"</TIMEX>")])
    | x `elem` dateWords = checkDateWords xs dateWords (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++"</TIMEX>")])
    | (length x) > 1 && (init x) `elem` dateWords = checkDateWords xs dateWords (result ++ [("<TIMEX TYPE=\"DATE\">" ++ (init x) ++"</TIMEX>" ++ [(last x)])])
    | (isDateNumber x) == True && (head xs) `elem` dateWords = checkDateWords (init xs) dateWords (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++ (head xs) ++"</TIMEX>")])
    | (isDateNumber x) == True && (init (head xs)) `elem` dateWords = checkDateWords (init xs) dateWords (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++ " " ++ (init (head xs)) ++"</TIMEX>" ++ [(last (head xs))] )])
    | otherwise = checkDateWords xs dateWords (result ++ [x])

--Return true if the number contained in [1..31]
isDateNumber string = if string `elem` ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"] then True else False

--Money at the end of the sentence.
--Money in the sentence.
checkForMoney :: [String] -> [String] -> [String]
checkForMoney [] x = x
checkForMoney (x:xs) result
    | (length x) > 1 && (head x) `elem` ['$', '₤', '¥'] && (init (head xs)) `elem` ["billion", "million", "thousand", "hundred"] = checkForMoney (tail xs) (result ++ [("<NUMEX TYPE=\"MONEY\">" ++ x ++ " " ++ (init (head xs)) ++"</NUMEX>" ++ [(last (head xs))])])
    | (length x) > 1 && (head x) `elem` ['$', '₤', '¥'] && (head xs) `elem` ["billion", "million", "thousand", "hundred"] = checkForMoney (init xs) (result ++ [("<NUMEX TYPE=\"MONEY\">" ++ x ++ " " ++ (head xs) ++"</NUMEX>")])
    | (length x) > 1 && (head x) `elem` ['$', '₤', '¥'] = checkForMoney xs (result ++ [("<NUMEX TYPE=\"MONEY\">" ++ x ++"</NUMEX>")])
    | otherwise = checkForMoney xs (result ++ [x])

--Organizations at the end of the sentence.
--Organizations in the sentence.
checkForOrganizations :: [String] -> [String] -> [String]
checkForOrganizations [] x = x
checkForOrganizations (x:xs) result
    | (length organizationList) > 2 && (head organizationList) `elem` ["of", "Of", "The", "the"] && (last (last organizationList)) `elem` ['.', ','] = checkForOrganizations (drop (length organizationList) (x:xs)) (result ++ [(head organizationList)] ++ [("<ENAMEX TYPE=\"ORGANIZATION\">" ++ (asList (init (tail organizationList))) ++ " " ++ (init (last organizationList)) ++ "</ENAMEX>" ++ [(last (last organizationList))])])
    | (length organizationList) > 2 && (head organizationList) `elem` ["of", "Of", "The", "the"] = checkForOrganizations (drop (length organizationList) (x:xs)) (result ++ [(head organizationList)] ++ [("<ENAMEX TYPE=\"ORGANIZATION\">" ++ (asList (tail organizationList)) ++ "</ENAMEX>")])
    | otherwise = checkForOrganizations xs (result ++ [x])
    where organizationList = (formOrganizationList (x:xs) [])

--Create organization list.
formOrganizationList words org = if (null words) == False
                                    then if (length (head words)) > 1 && ((head (head words)) `elem` ['A' .. 'Z'] || (head words) `elem` ["of", "the", "Inc"])
                                        then formOrganizationList (tail words) (org ++ [(head words)])
                                        else if (length org) < 2
                                            then []
                                            else org
                                        else if (length org) < 2
                                            then []
                                            else org

--Extract tags from text.
getTags :: [String] -> [String] -> [String]
getTags [] x = x
getTags (x:xs) confirmedTags = (getTags xs (confirmedTags ++ (getTagsFromItem (splitOn " " x) [])))

getTagsFromItem :: [String] -> [String] -> [String]
getTagsFromItem [] x = x
getTagsFromItem (x:xs) confirmedTags
    | (x == "<ENAMEX" || x == "<TIMEX" || x == "<NUMEX")  = (getTagsFromItem (drop (length (splitOn " " tagContents)) (x:xs)) (confirmedTags ++ [tagContents]))
    | otherwise = getTagsFromItem (xs) confirmedTags
    where tagContents = (getTagContents (x:xs) "")

getTagContents :: [String] -> String -> String
getTagContents [] x = x
getTagContents (x:xs) tagContent
    | "</ENAMEX>" `isInfixOf` x = (tagContent ++ x)
    | "</TIMEX>" `isInfixOf` x = (tagContent ++ x)
    | "</NUMEX>" `isInfixOf` x = (tagContent ++ x)
    | otherwise = getTagContents xs (tagContent ++ x ++ " ")

--Calculate the total tags from the given tagged file.
countTagsFromLine :: [String] -> Int
countTagsFromLine [] = 0
countTagsFromLine (x:xs)
    | ("</ENAMEX>" `isInfixOf` x || "</TIMEX>" `isInfixOf` x || "</NUMEX>" `isInfixOf` x) = 1 + countTagsFromLine xs
    | otherwise = countTagsFromLine xs

--Calculate the total tags from my tagged file.
countTagsFromMyLine :: [String] -> Int
countTagsFromMyLine [] = 0
countTagsFromMyLine (x:xs)
    | ("</ENAMEX>" `isInfixOf` x || "</TIMEX>" `isInfixOf` x || "</NUMEX>" `isInfixOf` x) = 1 + countTagsFromMyLine xs
    | otherwise = countTagsFromMyLine xs

--Insert a space in between the lists in ss and concatenates the result.
asList :: [String] -> String
asList ss = (intercalate " " ss)
