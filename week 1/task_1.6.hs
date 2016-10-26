{-
Example (fictitious United Kingdom bank, sort code 12-34-56, account number 98765432):
IBAN:		GB82 WEST 1234 5698 7654 32	
Rearrange:		W E S T12345698765432 G B82	
Convert to integer:		3214282912345698765432161182	
Compute remainder:		3214282912345698765432161182	mod 97 = 1
-}

--Check that the total IBAN length is correct
--as per the country. If not, the IBAN is invalid
lengthIBAN "" = 0
lengthIBAN (x:xs) = 1 + lengthIBAN xs

--Move the four initial characters to the end of the string
--若是要按照索引取得List中的元素，可以使用!!运算符，索引的下标为0
rearrangeIBAN (x:y:z:m:xs) = if lengthIBAN (x:y:z:m:xs) == 22 then (((xs ++ [x])++ [y]) ++ [z]) ++ [m] else "error"

--Replace each letter in the string with two digits, thereby 
--expanding the string, where A = 10, B = 11, ..., Z = 35
convertToInteger "" = ""
convertToInteger (x:xs)
    |rearrangeIBAN (x:xs) /= "error" && x == 'A' = "10" ++ convertToInteger xs
    |rearrangeIBAN (x:xs) /= "error" && x == 'B' = "11" ++ convertToInteger xs
    |rearrangeIBAN (x:xs) /= "error" && x == 'E' = "14" ++ convertToInteger xs
    |rearrangeIBAN (x:xs) /= "error" && x == 'G' = "16" ++ convertToInteger xs
    |rearrangeIBAN (x:xs) /= "error" && x == 'S' = "28" ++ convertToInteger xs
    |rearrangeIBAN (x:xs) /= "error" && x == 'T' = "29" ++ convertToInteger xs
    |rearrangeIBAN (x:xs) /= "error" && x == 'W' = "32" ++ convertToInteger xs
    |otherwise = x:convertToInteger xs
--Interpret the string as a decimal integer and compute the 
--remainder of that number on division by 97

--If the remainder is 1, the check digit test is passed 
--and the IBAN might be valid.

{-
import Data.Char (toUpper)
 
validateIBAN :: String -> Either String String
validateIBAN [] = Left "No IBAN number."
validateIBAN xs =
    case lookupCountry of
        Nothing -> invalidBecause "Country does not exist."
        Just l  -> if length normalized /= l
                        then invalidBecause "Number length does not match."
                        else check
    where
        -- remove blanks and make all letters uppercase
        normalized = map toUpper $ concat $ words xs
        -- get the country code
        country = take 2 normalized
        -- search number length
        lookupCountry = lookup country countries
        countries :: [(String, Int)]
        countries = zip (words "AL AT BE BA BG HR CZ DO FO FR DE GR GT \
            \IS IL KZ LV LI LU MT MU MD NL PK PL RO SA SK ES CH TR GB \
            \AD AZ BH BR CR CY DK EE FI GE GI GL HU IE IT KW LB LT MK \
            \MR MC ME NO PS PT SM RS SI SE TN AE VG")
            [28,20,16,20,22,21,24,28,18,27,22,27,28,26,23,20,21,21,20,
            31,30,24,18,24,28,24,24,24,24,21,26,22,24,28,22,29,21,28,18,
            20,18,22,23,18,28,22,27,30,28,20,19,27,27,22,15,29,25,27,22,
            19,24,24,23,24]
        digits = ['0'..'9']
        letters = ['A'..'Z']
        -- letters to be replaced
        replDigits = zip letters $ map show [10..35]
        -- digits and letters allowed in the IBAN number
        validDigits = digits ++ letters
        -- see if all digits and letters in the IBAN number are allowed
        sane = all (`elem` validDigits) normalized
        -- take the first 4 digits from the number and put them at its end
        (p1, p2) = splitAt 4 normalized
        p3 = p2 ++ p1
        -- convert the letters to numbers and
        -- convert the result to an integer
        p4 :: Integer
        p4 = read $ concat $ convertLetters p3
        convertLetters [] = []
        convertLetters (x:xs)
            | x `elem` digits = [x] : convertLetters xs
            | otherwise = let (Just ys) = lookup x replDigits
                          in  ys : convertLetters xs
        -- see if the number is valid
        check = if sane
                    then if p4 `mod` 97 == 1
                            then Right xs
                            else invalidBecause "Validation failed."
                    else invalidBecause "Number contains illegal digits."
 
        invalidBecause reason = Left $ "Invalid IBAN number " ++ xs ++
            ": " ++ reason
-}