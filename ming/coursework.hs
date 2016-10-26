import Data.List  
import Data.Char
import Utils
import System.Environment
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
recog contents = recog_organizations $lines $recog_time $lines $recog_date $lines $ recog_names $lines $recog_locations $lines $ recog_money $lines contents

time_patten= "([0-9]*:[0-2]*|[0-9]*:[0-9]*am|[0-9]*:[0-9]* am|[0-9]*am|[0-9]* am|[0-9]*:[0-9]*pm|[0-9]*:[0-9]* pm|[0-9]*pm|[0-9]* pm|[0-9]*:[0-9]* a.m. EST|[0-9]* a.m. EST|[0-9]* a.m. WST|[0-9]*:[0-9]* a.m. WST|[0-9]* p.m. EST|[0-9]* p.m. WST|[0-9]*:[0-9]* p.m. EST|[0-9]*:[0-9]* p.m. WST)"

recog_time :: [String] -> String
recog_time [] =""
recog_time (conts:next) =  (unlines_time (conts =~ time_patten :: (String,String,String))) ++ recog_time next

--unlines_time:: (String,String,String) -> String
unlines_time (s1,"","")  = s1
unlines_time (s1,s2,s3) = s1++ " <TIMEX TYPE=\"TIME\">" ++s2++ "</TIMEX>" ++ recog_time (lines s3)

date_patten_year ="([0-2][0-9][0-9][0-9]-[0-9]{1,2}-[0-9]{1,2})|([0-9]{1,2}-[0-9]{1,2}-[0-2][0-9][0-9][0-9])|([0-2][0-9][0-9][0-9])"
date_patten_month="|([0-9]{0,2}\\s{0,1}(January|February|March|April|May|June|July|August|September|October|November|December)\\s{0,1}([0-9]{0,2}))"
date_patten_week = "|((last|next|the end of|begining of|early of) Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)"
date_patten = date_patten_year ++ date_patten_month++date_patten_week

recog_date :: [String] -> String
recog_date [] =""
recog_date (conts:next) =  (unlines_date (conts =~ date_patten :: (String,String,String))) ++ recog_date next

--unlines_time:: (String,String,String) -> String
unlines_date (s1,"","")  = s1
unlines_date (s1,s2,s3) = s1++ " <TIMEX TYPE=\"DATE\">" ++s2++ "</TIMEX>" ++ recog_date (lines s3)

--good version
money_patten_float = "((\\$|€|£)[0-9]+.[0-9]+ (million|billion|thousand|trillion))"
money_patten_int = "|(\\$|€|£)[0-9]+.[0-9]+"

money_patten= money_patten_float ++ money_patten_int

--money_patten = "/[\\$\xA2-\xA5\\u058F\u060B\u09F2\u09F3\u09FB\u0AF1\u0BF9\u0E3F\u17DB\u20A0-\u20BD\uA838\uFDFC\uFE69\uFF04\uFFE0\uFFE1\uFFE5\uFFE6]/"
--money_patten = "(¤|฿|฿|Bs|Br|₵|¢|₡|₫|€|ƒ|Ft|Rs.|₲|₭|kr|£|₤|Lm|₥|₦|₱|P|Q|R|Sk|Rp|৳|R$|$|₸|₮|₩|¥|NT$|RMB|RMB¥|zł|₴|₪|៛|﷼|₽|RM)\\s{0,1}[0-9]*.{0,1}"
recog_money :: [String] -> String
recog_money [] =""
recog_money (conts:next) =  (unlines_money (conts =~ money_patten :: (String,String,String))) ++ recog_money next

--unlines_time:: (String,String,String) -> String
unlines_money (s1,"","")  = s1
unlines_money (s1,s2,s3) = s1++ "<NUMEX TYPE=\"MONEY\">" ++s2++ "</NUMEX>" ++ recog_money (lines s3)

-- surnames is extract from top 1000 surnames from website  http://names.mongabay.com/most_common_surnames.htm
surnames = ["SMITH" ,"JOHNSON" ,"WILLIAMS" ,"JONES","BROWN" ,"DAVIS" ,"MILLER" ,"WILSON" ,"MOORE" ,"TAYLOR" ,"ANDERSON" ,"THOMAS" ,"JACKSON" ,"WHITE" ,"HARRIS" ,"MARTIN" ,"THOMPSON" ,"GARCIA" ,"MARTINEZ" ,"ROBINSON" ,"CLARK" ,"RODRIGUEZ" ,"LEWIS" ,"LEE" ,"WALKER" ,"HALL" ,"ALLEN" ,"YOUNG" ,"HERNANDEZ" ,"KING" ,"WRIGHT" ,"LOPEZ" ,"HILL" ,"SCOTT" ,"GREEN" ,"ADAMS" ,"BAKER" ,"GONZALEZ" ,"NELSON" ,"CARTER" ,"MITCHELL" ,"PEREZ" ,"ROBERTS" ,"TURNER" ,"PHILLIPS" ,"CAMPBELL" ,"PARKER" ,"EVANS" ,"JOBS","EDWARDS" ,"COLLINS" ,"STEWART" ,"SANCHEZ" ,"MORRIS" ,"ROGERS" ,"REED" ,"COOK" ,"MORGAN" ,"BELL" ,"MURPHY" ,"BAILEY" ,"RIVERA" ,"COOPER" ,"RICHARDSON" ,"COX" ,"HOWARD" ,"WARD" ,"TORRES" ,"PETERSON" ,"GRAY" ,"RAMIREZ" ,"JAMES" ,"WATSON" ,"BROOKS" ,"KELLY" ,"SANDERS" ,"PRICE" ,"BENNETT" ,"WOOD" ,"BARNES" ,"ROSS" ,"HENDERSON" ,"COLEMAN" ,"JENKINS" ,"PERRY" ,"POWELL" ,"LONG" ,"PATTERSON" ,"HUGHES" ,"FLORES" ,"WASHINGTON" ,"BUTLER" ,"SIMMONS" ,"FOSTER" ,"GONZALES" ,"BRYANT" ,"ALEXANDER" ,"RUSSELL" ,"GRIFFIN" ,"DIAZ" ,"HAYES" ,"MYERS" ,"FORD" ,"HAMILTON" ,"GRAHAM" ,"SULLIVAN" ,"WALLACE" ,"WOODS" ,"COLE" ,"WEST" ,"JORDAN" ,"OWENS" ,"REYNOLDS" ,"FISHER" ,"ELLIS" ,"HARRISON" ,"GIBSON" ,"MCDONALD" ,"CRUZ" ,"MARSHALL" ,"ORTIZ" ,"GOMEZ" ,"MURRAY" ,"FREEMAN" ,"WELLS" ,"WEBB" ,"SIMPSON" ,"STEVENS" ,"TUCKER" ,"PORTER" ,"HUNTER" ,"HICKS" ,"CRAWFORD" ,"HENRY" ,"BOYD" ,"MASON" ,"MORALES" ,"KENNEDY" ,"WARREN" ,"DIXON" ,"RAMOS" ,"REYES" ,"BURNS" ,"GORDON" ,"SHAW" ,"HOLMES" ,"RICE" ,"ROBERTSON" ,"HUNT" ,"BLACK" ,"DANIELS" ,"PALMER" ,"MILLS" ,"NICHOLS" ,"GRANT" ,"KNIGHT" ,"FERGUSON" ,"ROSE" ,"STONE" ,"HAWKINS" ,"DUNN" ,"PERKINS" ,"HUDSON" ,"SPENCER" ,"GARDNER" ,"STEPHENS" ,"PAYNE" ,"PIERCE" ,"BERRY" ,"MATTHEWS" ,"ARNOLD" ,"WAGNER" ,"WILLIS" ,"RAY" ,"WATKINS" ,"OLSON" ,"CARROLL" ,"DUNCAN" ,"SNYDER" ,"HART" ,"CUNNINGHAM" ,"BRADLEY" ,"LANE" ,"ANDREWS" ,"RUIZ" ,"HARPER" ,"FOX" ,"RILEY" ,"ARMSTRONG" ,"CARPENTER" ,"WEAVER" ,"GREENE" ,"LAWRENCE" ,"ELLIOTT" ,"CHAVEZ" ,"SIMS" ,"AUSTIN" ,"PETERS" ,"KELLEY" ,"FRANKLIN" ,"LAWSON" ,"FIELDS" ,"GUTIERREZ" ,"RYAN" ,"SCHMIDT" ,"CARR" ,"VASQUEZ" ,"CASTILLO" ,"WHEELER" ,"CHAPMAN" ,"OLIVER" ,"MONTGOMERY" ,"RICHARDS" ,"WILLIAMSON" ,"JOHNSTON" ,"BANKS" ,"MEYER" ,"BISHOP" ,"MCCOY" ,"HOWELL" ,"ALVAREZ" ,"MORRISON" ,"HANSEN" ,"FERNANDEZ" ,"GARZA" ,"HARVEY" ,"LITTLE" ,"BURTON" ,"STANLEY" ,"NGUYEN" ,"GEORGE" ,"JACOBS" ,"REID" ,"KIM" ,"FULLER" ,"LYNCH" ,"DEAN" ,"GILBERT" ,"GARRETT" ,"ROMERO" ,"WELCH" ,"LARSON" ,"FRAZIER" ,"BURKE" ,"HANSON" ,"DAY" ,"MENDOZA" ,"MORENO" ,"BOWMAN" ,"MEDINA" ,"FOWLER" ,"BREWER" ,"HOFFMAN" ,"CARLSON" ,"SILVA" ,"PEARSON" ,"HOLLAND" ,"DOUGLAS" ,"FLEMING" ,"JENSEN" ,"VARGAS" ,"BYRD" ,"DAVIDSON" ,"HOPKINS" ,"MAY" ,"TERRY" ,"HERRERA" ,"WADE" ,"SOTO" ,"WALTERS" ,"CURTIS" ,"NEAL" ,"CALDWELL" ,"LOWE" ,"JENNINGS" ,"BARNETT" ,"GRAVES" ,"JIMENEZ" ,"HORTON" ,"SHELTON" ,"BARRETT" ,"OBRIEN" ,"CASTRO" ,"SUTTON" ,"GREGORY" ,"MCKINNEY" ,"LUCAS" ,"MILES" ,"CRAIG" ,"RODRIQUEZ" ,"CHAMBERS" ,"HOLT" ,"LAMBERT" ,"FLETCHER" ,"WATTS" ,"BATES" ,"HALE" ,"RHODES" ,"PENA" ,"BECK" ,"NEWMAN" ,"HAYNES" ,"MCDANIEL" ,"MENDEZ" ,"BUSH" ,"VAUGHN" ,"PARKS" ,"DAWSON" ,"SANTIAGO" ,"NORRIS" ,"HARDY" ,"LOVE" ,"STEELE" ,"CURRY" ,"POWERS" ,"SCHULTZ" ,"BARKER" ,"GUZMAN" ,"PAGE" ,"MUNOZ" ,"BALL" ,"KELLER" ,"CHANDLER" ,"WEBER" ,"LEONARD" ,"WALSH" ,"LYONS" ,"RAMSEY" ,"WOLFE" ,"SCHNEIDER" ,"MULLINS" ,"BENSON" ,"SHARP" ,"BOWEN" ,"DANIEL" ,"BARBER" ,"CUMMINGS" ,"HINES" ,"BALDWIN" ,"GRIFFITH" ,"VALDEZ" ,"HUBBARD" ,"SALAZAR" ,"REEVES" ,"WARNER" ,"STEVENSON" ,"BURGESS" ,"SANTOS" ,"TATE" ,"CROSS" ,"GARNER" ,"MANN" ,"MACK" ,"MOSS" ,"THORNTON" ,"DENNIS" ,"MCGEE" ,"FARMER" ,"DELGADO" ,"AGUILAR" ,"VEGA" ,"GLOVER" ,"MANNING" ,"COHEN" ,"HARMON" ,"RODGERS" ,"ROBBINS" ,"NEWTON" ,"TODD" ,"BLAIR" ,"HIGGINS" ,"INGRAM" ,"REESE" ,"CANNON" ,"STRICKLAND" ,"TOWNSEND" ,"POTTER" ,"GOODWIN" ,"WALTON" ,"ROWE" ,"HAMPTON" ,"ORTEGA" ,"PATTON" ,"SWANSON" ,"JOSEPH" ,"FRANCIS" ,"GOODMAN" ,"MALDONADO" ,"YATES" ,"BECKER" ,"ERICKSON" ,"HODGES" ,"RIOS" ,"CONNER" ,"ADKINS" ,"WEBSTER" ,"FRANK" ,"LOGAN" ,"OWEN" ,"BASS" ,"MARSH" ,"DRAKE" ,"WONG" ,"JEFFERSON" ,"PARK" ,"MORTON" ,"ABBOTT" ,"SPARKS" ,"PATRICK" ,"NORTON" ,"HUFF" ,"CLAYTON" ,"MASSEY" ,"LLOYD" ,"FIGUEROA" ,"CARSON" ,"BOWERS" ,"ROBERSON" ,"BARTON" ,"TRAN" ,"LAMB" ,"HARRINGTON" ,"CASEY" ,"BOONE" ,"CORTEZ" ,"CLARKE" ,"MATHIS" ,"SINGLETON" ,"WILKINS" ,"CAIN" ,"BRYAN" ,"UNDERWOOD" ,"HOGAN" ,"MCKENZIE" ,"COLLIER" ,"LUNA" ,"PHELPS" ,"MCGUIRE" ,"ALLISON" ,"BRIDGES" ,"WILKERSON" ,"NASH" ,"SUMMERS" ,"ATKINS" ,"WILCOX" ,"PITTS" ,"CONLEY" ,"MARQUEZ" ,"BURNETT" ,"RICHARD" ,"COCHRAN" ,"CHASE" ,"DAVENPORT" ,"HOOD" ,"GATES" ,"CLAY" ,"AYALA" ,"SAWYER" ,"ROMAN" ,"VAZQUEZ" ,"DICKERSON" ,"HODGE" ,"ACOSTA" ,"FLYNN" ,"ESPINOZA" ,"NICHOLSON" ,"MONROE" ,"WOLF" ,"MORROW" ,"KIRK" ,"RANDALL" ,"ANTHONY" ,"WHITAKER" ,"OCONNOR" ,"SKINNER" ,"WARE" ,"MOLINA" ,"KIRBY" ,"HUFFMAN" ,"BRADFORD" ,"CHARLES","MEHTA"]
firstnames = ["JAMES","JOHN","ROBERT","MICHAEL","WILLIAM","DAVID","RICHARD","CHARLES","JOSEPH","THOMAS","CHRISTOPHER","DANIEL","PAUL","MARK","DONALD","GEORGE","KENNETH","STEVEN","EDWARD","BRIAN","RONALD","ANTHONY","KEVIN","JASON","MATTHEW","GARY","TIMOTHY","JOSE","LARRY","JEFFREY","FRANK","SCOTT","ERIC","STEPHEN","ANDREW","RAYMOND","GREGORY","JOSHUA","JERRY","DENNIS","WALTER","PATRICK","PETER","HAROLD","DOUGLAS","HENRY","CARL","RYAN","ROGER","JOE","JUAN","JACK","ALBERT","JONATHAN","JUSTIN","TERRY","GERALD","KEITH","SAMUEL","WILLIE","RALPH","LAWRENCE","NICHOLAS","ROY","BENJAMIN","BRUCE","BRANDON","ADAM","HARRY","FRED","WAYNE","BILLY","STEVE","LOUIS","JEREMY","AARON","RANDY","HOWARD","EUGENE","CARLOS","RUSSELL","BOBBY","VICTOR","MARTIN","ERNEST","PHILLIP","TODD","JESSE","CRAIG","ALAN","SHAWN","CLARENCE","SEAN","PHILIP","CHRIS","JOHNNY","EARL","JIMMY","ANTONIO","DANNY","BRYAN","TONY","LUIS","MIKE","STANLEY","LEONARD","NATHAN","DALE","MANUEL","RODNEY","CURTIS","NORMAN","ALLEN","MARVIN","VINCENT","GLENN","JEFFERY","TRAVIS","JEFF","CHAD","JACOB","LEE","MELVIN","ALFRED","KYLE","FRANCIS","BRADLEY","JESUS","HERBERT","FREDERICK","RAY","JOEL","EDWIN","DON","EDDIE","RICKY","TROY","RANDALL","BARRY","ALEXANDER","BERNARD","MARIO","LEROY","FRANCISCO","MARCUS","MICHEAL","THEODORE","CLIFFORD","MIGUEL","OSCAR","JAY","JIM","TOM","CALVIN","ALEX","JON","RONNIE","BILL","LLOYD","TOMMY","LEON","DEREK","WARREN","DARRELL","JEROME","FLOYD","LEO","ALVIN","TIM","WESLEY","GORDON","DEAN","GREG","JORGE","DUSTIN","PEDRO","DERRICK","DAN","LEWIS","ZACHARY","COREY","HERMAN","MAURICE","VERNON","ROBERTO","CLYDE","GLEN","HECTOR","SHANE","RICARDO","SAM","RICK","LESTER","BRENT","RAMON","CHARLIE","TYLER","GILBERT","GENE","MARC","REGINALD","RUBEN","BRETT","ANGEL","NATHANIEL","RAFAEL","LESLIE","EDGAR","MILTON","RAUL","BEN","CHESTER","CECIL","DUANE","FRANKLIN","ANDRE","ELMER","BRAD","GABRIEL","RON","MITCHELL","ROLAND","ARNOLD","HARVEY","JARED","ADRIAN","KARL","CORY","CLAUDE","ERIK","DARRYL","JAMIE","NEIL","JESSIE","CHRISTIAN","JAVIER","FERNANDO","CLINTON","TED","MATHEW","TYRONE","DARREN","LONNIE","LANCE","CODY","JULIO","KELLY","KURT","ALLAN","NELSON","GUY","CLAYTON","HUGH","MAX","DWAYNE","DWIGHT","ARMANDO","FELIX","JIMMIE","EVERETT","JORDAN","IAN","WALLACE","KEN","BOB","JAIME","CASEY","ALFREDO","ALBERTO","DAVE","IVAN","JOHNNIE","SIDNEY","BYRON","JULIAN","ISAAC","MORRIS","CLIFTON","WILLARD","DARYL","ROSS","VIRGIL","ANDY","MARSHALL","SALVADOR","PERRY","KIRK","SERGIO","MARION","TRACY","SETH","KENT","TERRANCE","RENE","EDUARDO","TERRENCE","ENRIQUE","FREDDIE","WADE","AUSTIN","STUART","FREDRICK","ARTURO","ALEJANDRO","JACKIE","JOEY","NICK","LUTHER","WENDELL","JEREMIAH","EVAN","JULIUS","DANA","DONNIE","OTIS","SHANNON","TREVOR","OLIVER","LUKE","HOMER","GERARD","DOUG","KENNY","HUBERT","ANGELO","SHAUN","LYLE","MATT","LYNN","ALFONSO","ORLANDO","REX","CARLTON","ERNESTO","CAMERON","NEAL","PABLO","LORENZO","OMAR","WILBUR","BLAKE","GRANT","HORACE","RODERICK","KERRY","ABRAHAM","WILLIS","RICKEY","JEAN","IRA","ANDRES","CESAR","JOHNATHAN","MALCOLM","RUDOLPH","DAMON","KELVIN","RUDY","PRESTON","ALTON","ARCHIE","MARCO","WM","PETE","RANDOLPH","GARRY","GEOFFREY","JONATHON","FELIPE","BENNIE","GERARDO","ED","DOMINIC","ROBIN","LOREN","DELBERT","COLIN","GUILLERMO","EARNEST","LUCAS","BENNY","NOEL","SPENCER","RODOLFO","MYRON","EDMUND","GARRETT","SALVATORE","CEDRIC","LOWELL","GREGG","SHERMAN","WILSON","DEVIN","SYLVESTER","KIM","ROOSEVELT","ISRAEL","JERMAINE","FORREST","WILBERT","LELAND","SIMON","GUADALUPE","CLARK","IRVING","CARROLL","BRYANT","OWEN","RUFUS","WOODROW","SAMMY","KRISTOPHER","MACK","LEVI","MARCOS","GUSTAVO","JAKE","LIONEL","MARTY","TAYLOR","ELLIS","DALLAS","GILBERTO","CLINT","NICOLAS","LAURENCE","ISMAEL","ORVILLE","DREW","JODY","ERVIN","DEWEY","AL","WILFRED","JOSH","HUGO","IGNACIO","CALEB","TOMAS","SHELDON","ERICK","FRANKIE","STEWART","DOYLE","DARREL","ROGELIO","TERENCE","SANTIAGO","ALONZO","ELIAS","BERT","ELBERT","RAMIRO","CONRAD","PAT","NOAH","GRADY","PHIL","CORNELIUS","LAMAR","ROLANDO","CLAY","PERCY","DEXTER","BRADFORD","MERLE","DARIN","AMOS","TERRELL","MOSES","IRVIN","SAUL","ROMAN","DARNELL","RANDAL","TOMMIE","TIMMY","DARRIN","WINSTON","BRENDAN","TOBY","VAN","ABEL","DOMINICK","BOYD","COURTNEY","JAN","EMILIO","ELIJAH","CARY","DOMINGO","SANTOS","AUBREY","EMMETT","MARLON","EMANUEL","JERALD","EDMOND","MARY","PATRICIA","LINDA","BARBARA","ELIZABETH","JENNIFER","MARIA","SUSAN","MARGARET","DOROTHY","LISA","NANCY","KAREN","BETTY","HELEN","SANDRA","DONNA","CAROL","RUTH","SHARON","MICHELLE","LAURA","SARAH","KIMBERLY","DEBORAH","JESSICA","SHIRLEY","CYNTHIA","ANGELA","MELISSA","BRENDA","AMY","ANNA","REBECCA","VIRGINIA","KATHLEEN","PAMELA","MARTHA","DEBRA","AMANDA","STEPHANIE","CAROLYN","CHRISTINE","MARIE","JANET","CATHERINE","FRANCES","ANN","JOYCE","DIANE","ALICE","JULIE","HEATHER","TERESA","DORIS","GLORIA","EVELYN","JEAN","CHERYL","MILDRED","KATHERINE","JOAN","ASHLEY","JUDITH","ROSE","JANICE","KELLY","NICOLE","JUDY","CHRISTINA","KATHY","THERESA","BEVERLY","DENISE","TAMMY","IRENE","JANE","LORI","RACHEL","MARILYN","ANDREA","KATHRYN","LOUISE","SARA","ANNE","JACQUELINE","WANDA","BONNIE","JULIA","RUBY","LOIS","TINA","PHYLLIS","NORMA","PAULA","DIANA","ANNIE","LILLIAN","EMILY","ROBIN","PEGGY","CRYSTAL","GLADYS","RITA","DAWN","CONNIE","FLORENCE","TRACY","EDNA","TIFFANY","CARMEN","ROSA","CINDY","GRACE","WENDY","VICTORIA","EDITH","KIM","SHERRY","SYLVIA","JOSEPHINE","THELMA","SHANNON","SHEILA","ETHEL","ELLEN","ELAINE","MARJORIE","CARRIE","CHARLOTTE","MONICA","ESTHER","PAULINE","EMMA","JUANITA","ANITA","RHONDA","HAZEL","AMBER","EVA","DEBBIE","APRIL","LESLIE","CLARA","LUCILLE","JAMIE","JOANNE","ELEANOR","VALERIE","DANIELLE","MEGAN","ALICIA","SUZANNE","MICHELE","GAIL","BERTHA","DARLENE","VERONICA","JILL","ERIN","GERALDINE","LAUREN","CATHY","JOANN","LORRAINE","LYNN","SALLY","REGINA","ERICA","BEATRICE","DOLORES","BERNICE","AUDREY","YVONNE","ANNETTE","JUNE","SAMANTHA","MARION","DANA","STACY","ANA","RENEE","IDA","VIVIAN","ROBERTA","HOLLY","BRITTANY","MELANIE","LORETTA","YOLANDA","JEANETTE","LAURIE","KATIE","KRISTEN","VANESSA","ALMA","SUE","ELSIE","BETH","JEANNE","VICKI","CARLA","TARA","ROSEMARY","EILEEN","TERRI","GERTRUDE","LUCY","TONYA","ELLA","STACEY","WILMA","GINA","KRISTIN","JESSIE","NATALIE","AGNES","VERA","WILLIE","CHARLENE","BESSIE","DELORES","MELINDA","PEARL","ARLENE","MAUREEN","COLLEEN","ALLISON","TAMARA","JOY","GEORGIA","CONSTANCE","LILLIE","CLAUDIA","JACKIE","MARCIA","TANYA","NELLIE","MINNIE","MARLENE","HEIDI","GLENDA","LYDIA","VIOLA","COURTNEY","MARIAN","STELLA","CAROLINE","DORA","JO","VICKIE","MATTIE","TERRY","MAXINE","IRMA","MABEL","MARSHA","MYRTLE","LENA","CHRISTY","DEANNA","LYNDA","MADELINE","AMELIA","ALBERTA","GENEVIEVE","MONIQUE","JODI","VERDI","JANIE","MAGGIE","KAYLA","SONYA","JAN","LEE","KRISTINE","CANDACE","FANNIE","ZARIN"]

recog_names :: [String] -> String
recog_names [] =""
recog_names (conts:next) =  (unlines_names (conts =~ name_patten :: (String,String,String))) ++ recog_names  next

--unlines_time:: (String,String,String) -> String
unlines_names (s1,"","")  = s1
unlines_names (s1,s2,s3) = s1++ "<ENAMEX TYPE=\"PERSON\">" ++s2++ "</ENAMEX>" ++ recog_names (lines s3)

construct_patten str = replace "]" "" $replace "[" "" $replace "\"" "" (replace "," "|" str)
name_patten = (construct_patten $show getFullNameList) ++"|"++ (construct_patten $show (map stringToTitle firstnames)) ++"|"++ (construct_patten $show (map stringToTitle surnames)) 

getFullNameList = [fst++" "++sur|fst <-(map stringToTitle firstnames), sur <- (map stringToTitle surnames)]

stringToTitle :: String -> String
stringToTitle "" = ""
stringToTitle (x:xs) = x:(map toLower xs)

locations_list = ["Afghanistan","Albania","Algeria","American Samoa","Andorra","Angola","Anguilla","Antigua and Barbuda","South America","Armenia","Aruba","Australia","Austria","Azerbaijan","Bahamas","Middle East","Bangladesh","Barbados","Belarus","Belgium","North America","Benin","Bermuda","Bhutan","South America","Bonaire","Bosnia-Herzegovina","Botswana","Bouvet Island","South America","Brunei","Bulgaria","Burkina Faso","Burundi","Cambodia","Cameroon","North America","Cape Verde","Cayman Islands","Central African Republic","California","Chad","South America","China","Christmas Island","Cocos (Keeling) Islands","South America","Comoros","Congo, Democratic Republic of the (Zaire)","Congo, Republic of","Cook Islands","North America","Croatia","Cuba","Curacao","Cyprus","Czech Republic","Denmark","Djibouti","Dominica","Dominican Republic","South America","Egypt","El Salvador","North America","Equatorial Guinea","Eritrea","Estonia","Ethiopia","South America","Faroe Islands","Fiji","Finland","France","South America","Gabon","Gambia","Georgia","Germany","Ghana","Gibraltar","Greece","Greenland","Grenada","Guadeloupe (French)","Guam (USA)","North America","Guinea","Guinea Bissau","South America","Haiti","Holy See","North America","Hong Kong","Hungary","Iceland","India","Indonesia","Middle East","Middle East","Ireland","Middle East","Italy","Ivory Coast (Cote D`Ivoire)","Jamaica","Japan","Middle East","Kazakhstan","Kenya","Kiribati","Kosovo","Middle East","Kyrgyzstan","Laos","Latvia","Middle East","Lesotho","Liberia","Libya","Liechtenstein","Lithuania","Luxembourg","Macau","Macedonia","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Marshall Islands","Martinique (French)","Mauritania","Mauritius","Mayotte","North America","Micronesia","Moldova","Monaco","Mongolia","Montenegro","Montserrat","Morocco","Mozambique","Myanmar","Namibia","Nauru","Nepal","Netherlands","Netherlands Antilles","New Caledonia (French)","New Zealand","North America","Niger","Nigeria","Niue","Norfolk Island","North Korea","Northern Mariana Islands","Norway","Middle East","Pakistan","Palau","North America","Papua New Guinea","South America","South America","Philippines","Pitcairn Island","Poland","Polynesia (French)","Portugal","Puerto Rico","Middle East","Reunion","Romania","Russia","Rwanda","Saint Helena","Saint Kitts and Nevis","Saint Lucia","North America","Saint Vincent and Grenadines","Samoa","San Marino","Sao Tome and Principe","Middle East","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Sint Maarten","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","South America","South Korea","South Sudan","South Sudan","Spain","Sri Lanka","Sudan","South America","Svalbard and Jan Mayen Islands","Swaziland","Sweden","Switzerland","Middle East","Taiwan","Tajikistan","Tanzania","Thailand","Timor-Leste (East Timor)","Togo","Tokelau","Tonga","Trinidad and Tobago","Tunisia","Middle East","Turkmenistan","Turks and Caicos Islands","Tuvalu","Uganda","Ukraine","Middle East","United Kingdom","North America","South America","Uzbekistan","Vanuatu","South America","Vietnam","Virgin Islands","Wallis and Futuna Islands","Middle East","Zambia","Zimbabwe","xk","Asia","Europe","Africa","Australisia","Caribbean","South America","North America"]

recog_locations :: [String] -> String
recog_locations [] =""
recog_locations (conts:next) =  (unlines_locations (conts =~ location_patten :: (String,String,String))) ++ recog_money next

--unlines_time:: (String,String,String) -> String
unlines_locations (s1,"","")  = s1
unlines_locations (s1,s2,s3) = s1++ "<ENAMEX TYPE=\"LOCATION\">" ++s2++ "</ENAMEX>" ++ recog_locations (lines s3)

location_patten =  construct_patten $show locations_list

--http://topyaps.com/top-10-international-organizations
organizations_list = ["North Atlantic Treaty Organization (NATO)","United Nations (UN)","Group of 8 (G8)","World Trade Organization (WTO)","World Bank","United Nations Educational","Scientific and Cultural Organization (UNESCO)","United Nations Children’s Fund (UNICEF)","World Health Organization (WHO)","World Wildlife Fund (WWF)","International Monetary Fund (IMF)"]

recog_organizations :: [String] -> String
recog_organizations [] =""
recog_organizations (conts:next) =  (unlines_locations (conts =~ location_patten :: (String,String,String))) ++ recog_organizations next

--unlines_time:: (String,String,String) -> String
unlines_organizations (s1,"","")  = s1
unlines_organizations (s1,s2,s3) = s1++ "<ENAMEX TYPE=\"ORGANIZATION\">" ++s2++ "</ENAMEX>" ++ recog_organizations  (lines s3)

organizations_patten =  construct_patten $show organizations_list