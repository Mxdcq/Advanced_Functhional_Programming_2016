{-
    BNF Converter: Latex Generator
    Copyright (C) 2004  Author:  Markus Forberg, Aarne Ranta

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module BNFC.Backend.Txt2Tag (cfToTxt)where

import BNFC.CF
import AbsBNF (Reg (..))
import BNFC.Utils
import Data.List

cfToTxt :: String -> CF -> String
cfToTxt name cf = unlines [
                            beginDocument name,
                            introduction,
                            prtTerminals name cf,
                            prtBNF name cf
                            ]

introduction :: String
introduction = concat
               [
               "\nThis document was automatically generated by ",
               "the //BNF-Converter//.",
               " It was generated together with the lexer, the parser, and the",
               " abstract syntax module, which guarantees that the document",
               " matches with the implementation of the language (provided no",
               " hand-hacking has taken place).\n"
               ]

prtTerminals :: String -> CF -> String
prtTerminals name cf = unlines [
                               "==The lexical structure of " ++ name ++ "==",
                               identSection cf,
                               "===Literals===",
                               prtLiterals name cf,
                               unlines (map prtOwnToken (tokenPragmas cf)),
                               "===Reserved words and symbols===",
                               prtReserved name cf,
                               prtSymb name cf,
                               "===Comments===",
                               prtComments $ comments cf
                               ]

identSection cf = if not (hasIdent cf) then [] else
                    unlines [
                               "===Identifiers===",
                               prtIdentifiers
                          ]

prtIdentifiers :: String
prtIdentifiers = unlines
  [
   "Identifiers //Ident// are unquoted strings beginning with a letter,",
   "followed by any combination of letters, digits, and the characters ``_ '``",
   "reserved words excluded."
  ]

prtLiterals :: String -> CF -> String
prtLiterals _ cf =
  unlines $ map stringLit $
    filter (`notElem` [Cat "Ident"]) $
      literals cf

stringLit :: Cat -> String
stringLit cat = unlines $ case show cat of
  "Char" -> ["Character literals //Char// have the form",
             "``'``//c//``'``, where //c// is any single character.",
             ""
            ]
  "String" -> ["String literals //String// have the form",
             "``\"``//x//``\"``}, where //x// is any sequence of any characters",
             "except ``\"`` unless preceded by ``\\``.",
             ""]
  "Integer" -> ["Integer literals //Integer// are nonempty sequences of digits.",
             ""]
  "Double" -> ["Double-precision float literals //Double// have the structure",
               "indicated by the regular expression" +++
               "``digit+ '.' digit+ ('e' ('-')? digit+)?`` i.e.\\",
               "two sequences of digits separated by a decimal point, optionally",
               "followed by an unsigned or negative exponent.",
               ""]
  _ -> []

prtOwnToken (name,reg) = unlines
  [show name +++ "literals are recognized by the regular expression",
   "```" ++
   latexRegExp reg ++
   "```"
  ]

prtComments :: ([(String,String)],[String]) -> String
prtComments (xs,ys) = concat
                   [
                   if null ys then
                    "There are no single-line comments in the grammar."
                   else
                    "Single-line comments begin with " ++ sing ++".",
                   if null xs then
                    "There are no multiple-line comments in the grammar."
                   else
                   "Multiple-line comments are  enclosed with " ++ mult ++"."
                   ]
 where
 sing = intercalate ", " $ map (symbol.prt) ys
 mult = intercalate ", " $
         map (\(x,y) -> symbol (prt x) ++ " and " ++ symbol (prt y)) xs

prtSymb :: String -> CF -> String
prtSymb name cf = case symbols cf of
                   [] -> "\nThere are no symbols in " ++ name ++ ".\n"
                   xs -> "The symbols used in " ++ name ++ " are the following:\n"
                         ++
                         tabular 4 (three $ map (symbol.prt) xs)

prtReserved :: String -> CF -> String
prtReserved name cf = case reservedWords cf of
                       [] -> stringRes name ++
                             "\nThere are no reserved words in " ++ name ++ ".\n"
                       xs -> stringRes name ++
                             tabular 4 (three $ map quote xs)

stringRes :: String -> String
stringRes name = concat
                 ["The set of reserved words is the set of terminals ",
                  "appearing in the grammar. Those reserved words ",
                  "that consist of non-letter characters are called symbols, and ",
                  "they are treated in a different way from those that ",
                  "are similar to identifiers. The lexer ",
                  "follows rules familiar from languages ",
                  "like Haskell, C, and Java, including longest match ",
                  "and spacing conventions.",
                  "\n\n",
                  "The reserved words used in " ++ name ++ " are the following:\n"]

three :: [String] -> [[String]]
three []         = []
three [x]        = [[x,[],[],[]]]
three [x,y]      = [[x,y,[],[]]]
three [x,y,z]      = [[x,y,z,[]]]
three (x:y:z:u:xs) = [x,y,z,u] : three xs

prtBNF :: String -> CF -> String
prtBNF name cf = unlines [
                     "==The syntactic structure of " ++ name ++"==",
                     "Non-terminals are enclosed between < and >. ",
                     "The symbols " ++ arrow ++ " (production), " ++
                      delimiter ++" (union) ",
                     "and " ++ empty ++ " (empty rule) belong to the BNF notation. ",
                     "All other symbols are terminals.",
                     "",
                     prtRules (ruleGroups cf)
                     ]

prtRules :: [(Cat,[Rule])] -> String
prtRules          [] = []
prtRules ((c,[]):xs)
    = tabular 3 [[nonterminal c,arrow,[]]] ++ prtRules xs
prtRules ((c, r : rs) : xs)
    = tabular 3 ([[nonterminal c,arrow,prtSymbols $ rhsRule r]] ++
                 [[[],delimiter,prtSymbols (rhsRule y)] | y <-  rs]) ++
    ---  "\n\n" ++ --- with empty lines good for latex, bad for html
      prtRules xs

prtSymbols :: [Either Cat String] -> String
prtSymbols [] = empty
prtSymbols xs = foldr ((+++) . p) [] xs
 where p (Left  r) = nonterminal r
       p (Right r) = terminal r

prt :: String -> String
prt s = s

empty :: String
empty = "**eps**"

symbol :: String -> String
symbol s = s

tabular :: Int -> [[String]] -> String
tabular _ xs = unlines [unwords (intersperse "|" (" " : x)) | x <- xs]

terminal :: String -> String
terminal s = "``" ++ s ++ "``"

nonterminal :: Cat -> String
nonterminal s = "//" ++ show s ++ "//"

arrow :: String
arrow = "->"

delimiter :: String
delimiter = " **|** "

beginDocument :: String -> String
beginDocument name = unlines [
 "The Language " ++ name,
 "BNF Converter",
 "",
 "",
 "%This txt2tags file is machine-generated by the BNF-converter",
 "%Process by txt2tags to generate html or latex",
 ""
 ]

latexRegExp :: Reg -> String
latexRegExp = quote . rex (0 :: Int) where
  rex i e = case e of
    RSeq reg0 reg  -> ifPar i 2 $ rex 2 reg0 +++ rex 2 reg
    RAlt reg0 reg  -> ifPar i 1 $ rex 1 reg0 +++ "|" +++ rex 1 reg
    RMinus reg0 reg  -> ifPar i 1 $ rex 2 reg0 +++ "-" +++ rex 2 reg
    RStar reg  -> rex 3 reg ++ "*"
    RPlus reg  -> rex 3 reg ++ "+"
    ROpt reg  -> rex 3 reg ++ "?"
    REps  -> "eps"
    RChar c  -> "'" ++ [c] ++ "'"
    RAlts str  -> "[\"" ++ str ++ "\"]"
    RSeqs str  -> "{\"" ++ str ++ "\"}"
    RDigit  -> "digit"
    RLetter  -> "letter"
    RUpper  -> "upper"
    RLower  -> "lower"
    RAny  -> "char"
  ifPar i j s = if i > j then "(" ++ s ++ ")" else s

quote s = "``" ++ s ++ "``"