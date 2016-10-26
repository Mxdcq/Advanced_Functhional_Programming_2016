import System.Environment   
import Data.List  
import Data.Char
import System.IO
import System.Directory
import Text.Regex.Posix
import Text.Parsec
import Text.Regex.Base.Context

main = do 
  putStrLn "Please put the name of the tested file and put it in the current directory."
  fileName <- getLine
  fileExists <- doesFileExist fileName
  if fileExists
    then do contents <- readFile fileName
            writeFile "result.txt" (solveNER contents) 
		    
						  
	else do putStrLn "The file doesn't exist!"
	
solveNER:: String -> String	
solveNER contents =  recog contents

--recog:: String -> String
recog contents = recog_money $lines $recog_date $lines $recog_time $lines contents

time_patten= "([0-9]*:[0-2]*|[0-9]*:[0-9]*am|[0-9]*:[0-9]* am|[0-9]*am|[0-9]* am|[0-9]*:[0-9]*pm|[0-9]*:[0-9]* pm|[0-9]*pm|[0-9]* pm|[0-9]*:[0-9]* a.m. EST|[0-9]* a.m. EST|[0-9]* a.m. WST|[0-9]*:[0-9]* a.m. WST|[0-9]* p.m. EST|[0-9]* p.m. WST|[0-9]*:[0-9]* p.m. EST|[0-9]*:[0-9]* p.m. WST)"

recog_time :: [String] -> String
recog_time [] =""
recog_time (conts:next) =  (unlines_time (conts =~ time_patten :: (String,String,String))) ++ recog_time next

--unlines_time:: (String,String,String) -> String
unlines_time (s1,"","")  = s1
unlines_time (s1,s2,s3) = s1++ " <TIMEX TYPE=\"TIME\">" ++s2++ "</TIMEX>" ++ recog_time (lines s3)

--date_patten = "[0-2][0-9][0-9][0-9]|January|([0-9]* )February|March|April|May|June|July|August|September|October|November|December"

--date_patten = "([0-2][0-9][0-9][0-9]|[0-9]* --(January|February|March|April|May|June|July|August|September|October|November|December)|((January|February|March|April|May|June|July|August|September|October|November|December)--  [0-9]*(th|st|nd|rd))|)"

date_patten_year ="([0-2][0-9][0-9][0-9]-[0-9]{1,2}-[0-9]{1,2})|([0-9]{1,2}-[0-9]{1,2}-[0-2][0-9][0-9][0-9])|([0-2][0-9][0-9][0-9])"
date_patten_month="|([0-9]{0,2}\\s{0,1}(January|February|March|April|May|June|July|August|September|October|November|December)\\s{0,1}([0-9]{0,2}))"

date_patten = date_patten_year ++ date_patten_month

recog_date :: [String] -> String
recog_date [] =""
recog_date (conts:next) =  (unlines_date (conts =~ date_patten :: (String,String,String))) ++ recog_date next

--unlines_time:: (String,String,String) -> String
unlines_date (s1,"","")  = s1
unlines_date (s1,s2,s3) = s1++ " <TIMEX TYPE=\"DATE\">" ++s2++ "</TIMEX>" ++ recog_date (lines s3)

--good version
money_patten_float = "((\\$|€|£)[0-9]+.[0-9]+ (million|billion|thousand|trillion))"
money_patten_int = "|((\\$|€|£)[0-9]+ (million|billion|thousand|trillion))"

money_patten= money_patten_float ++ money_patten_int

--money_patten = "/[\\$\xA2-\xA5\\u058F\u060B\u09F2\u09F3\u09FB\u0AF1\u0BF9\u0E3F\u17DB\u20A0-\u20BD\uA838\uFDFC\uFE69\uFF04\uFFE0\uFFE1\uFFE5\uFFE6]/"

--money_patten = "(¤|฿|฿|Bs|Br|₵|¢|₡|₫|€|ƒ|Ft|Rs.|₲|₭|kr|£|₤|Lm|₥|₦|₱|P|Q|R|Sk|Rp|৳|R$|$|₸|₮|₩|¥|NT$|RMB|RMB¥|zł|₴|₪|៛|﷼|₽|RM)\\s{0,1}[0-9]*.{0,1}"
recog_money :: [String] -> String
recog_money [] =""
recog_money (conts:next) =  (unlines_money (conts =~ money_patten :: (String,String,String))) ++ recog_money next

--unlines_time:: (String,String,String) -> String
unlines_money (s1,"","")  = s1
unlines_money (s1,s2,s3) = s1++ "<NUMEX TYPE=\"MONEY\">" ++s2++ "</NUMEX>" ++ recog_money (lines s3)
	
-- words contents
--solveNER contents = recog_time $ words contents

--recog_time :: [String] -> [String]
--recog_time words = map foldl (\x ->if isTime x then "" else "") [] words

--let time= "([0-9]:[0-9]|*a.m.*|*p.m.*|*am*|*pm*)"

--isTime word = word  =~ time


	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	        