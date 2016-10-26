{-
    BNF Converter: Alex 2.0 Generator
    Copyright (C) 2004  Author:  Peter Gammie

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

-------------------------------------------------------------------
-- |
-- Module      :  BNFC.Backend.Haskell.CFtoAlex2
-- Copyright   :  (C)opyright 2003, {aarne,markus,peteg} at cs dot chalmers dot se
-- License     :  GPL (see COPYING for details)
--
-- Maintainer  :  {markus,aarne} at cs dot chalmers dot se
-- Stability   :  alpha
-- Portability :  Haskell98
--
-- Hacked version of @BNFC.Backend.Haskell.CFtoAlex@ to cope with Alex2.
--
-------------------------------------------------------------------
module BNFC.Backend.Haskell.CFtoAlex2 (cf2alex2) where

import BNFC.CF
import Data.List

-- For RegToAlex, see below.
import AbsBNF
import Data.Char

cf2alex2 :: String -> String -> String -> Bool -> Bool -> CF -> String
cf2alex2 name errMod shareMod shareStrings byteStrings cf =
  unlines $ intercalate [""] [
    prelude name errMod shareMod shareStrings byteStrings,
    cMacros,
    rMacros cf,
    restOfAlex shareMod shareStrings byteStrings cf
   ]

prelude :: String -> String -> String -> Bool -> Bool -> [String]
prelude name _ shareMod shareStrings byteStrings = [
  "-- -*- haskell -*-",
  "-- This Alex file was machine-generated by the BNF converter",
  "{",
  "{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}",
  "module " ++ name ++ " where",
  "",
  -- "import " ++ errMod,
  if shareStrings then "import " ++ shareMod else "",
  if byteStrings  then "import qualified Data.ByteString.Char8 as BS" else "",
  "}",
  ""
  ]

cMacros :: [String]
cMacros = [
  "$l = [a-zA-Z\\192 - \\255] # [\\215 \\247]    -- isolatin1 letter FIXME",
  "$c = [A-Z\\192-\\221] # [\\215]    -- capital isolatin1 letter FIXME",
  "$s = [a-z\\222-\\255] # [\\247]    -- small isolatin1 letter FIXME",
  "$d = [0-9]                -- digit",
  "$i = [$l $d _ ']          -- identifier character",
  "$u = [\\0-\\255]          -- universal: any character"
  ]

rMacros :: CF -> [String]
rMacros cf =
  let symbs = symbols cf
  in
  (if null symbs then [] else [
   "@rsyms =    -- symbols and non-identifier-like reserved words",
   "   " ++ unwords (intersperse "|" (map mkEsc symbs))
   ])
 where
  mkEsc = unwords . esc
  esc s = if null a then rest else show a : rest
      where (a,r) = span isAlphaNum s
            rest = case r of
                       [] -> []
                       (c:xs) -> s : esc xs
                         where s = if isPrint c then ['\\',c]
                                                else '\\':show (ord c)

restOfAlex :: String -> Bool -> Bool -> CF -> [String]
restOfAlex _ shareStrings byteStrings cf = [
  ":-",
  lexComments (comments cf),
  "$white+ ;",
  pTSpec (symbols cf),

  userDefTokenTypes,
  ident,

  ifC catString ("\\\" ([$u # [\\\" \\\\ \\n]] | (\\\\ (\\\" | \\\\ | \\' | n | t)))* \\\"" ++
                  "{ tok (\\p s -> PT p (TL $ share $ unescapeInitTail s)) }"),
  ifC catChar    "\\\' ($u # [\\\' \\\\] | \\\\ [\\\\ \\\' n t]) \\'  { tok (\\p s -> PT p (TC $ share s))  }",
  ifC catInteger "$d+      { tok (\\p s -> PT p (TI $ share s))    }",
  ifC catDouble  "$d+ \\. $d+ (e (\\-)? $d+)? { tok (\\p s -> PT p (TD $ share s)) }",
  "",
  "{",
  "",
  "tok f p s = f p s",
  "",
  "share :: "++stringType++" -> "++stringType,
  "share = " ++ if shareStrings then "shareString" else "id",
  "",
  "data Tok =",
  "   TS !"++stringType++" !Int    -- reserved words and symbols",
  " | TL !"++stringType++"         -- string literals",
  " | TI !"++stringType++"         -- integer literals",
  " | TV !"++stringType++"         -- identifiers",
  " | TD !"++stringType++"         -- double precision float literals",
  " | TC !"++stringType++"         -- character literals",
  userDefTokenConstrs,
  " deriving (Eq,Show,Ord)",
  "",
  "data Token = ",
  "   PT  Posn Tok",
  " | Err Posn",
  "  deriving (Eq,Show,Ord)",
  "",
  "tokenPos (PT (Pn _ l _) _ :_) = \"line \" ++ show l",
  "tokenPos (Err (Pn _ l _) :_) = \"line \" ++ show l",
  "tokenPos _ = \"end of file\"",
  "",
  "posLineCol (Pn _ l c) = (l,c)",
  "mkPosToken t@(PT p _) = (posLineCol p, prToken t)",
  "",
  "prToken t = case t of",
  "  PT _ (TS s _) -> s",
  "  PT _ (TL s)   -> show s",
  "  PT _ (TI s)   -> s",
  "  PT _ (TV s)   -> s",
  "  PT _ (TD s)   -> s",
  "  PT _ (TC s)   -> s",
  userDefTokenPrint,
  "",
  "data BTree = N | B "++stringType++" Tok BTree BTree deriving (Show)",
  "",
  "eitherResIdent :: ("++stringType++" -> Tok) -> "++stringType++" -> Tok",
  "eitherResIdent tv s = treeFind resWords",
  "  where",
  "  treeFind N = tv s",
  "  treeFind (B a t left right) | s < a  = treeFind left",
  "                              | s > a  = treeFind right",
  "                              | s == a = t",
  "",
  "resWords = " ++ show (sorted2tree $ zip (sort resws) [1..]),
  "   where b s n = let bs = "++stringPack++" s",
  "                  in B bs (TS bs n)",
  "",
  "unescapeInitTail :: "++stringType++" -> "++stringType++"",
  "unescapeInitTail = "++stringPack++" . unesc . tail . "++stringUnpack++" where",
  "  unesc s = case s of",
  "    '\\\\':c:cs | elem c ['\\\"', '\\\\', '\\\''] -> c : unesc cs",
  "    '\\\\':'n':cs  -> '\\n' : unesc cs",
  "    '\\\\':'t':cs  -> '\\t' : unesc cs",
  "    '\"':[]    -> []",
  "    c:cs      -> c : unesc cs",
  "    _         -> []",
  "",
  "-------------------------------------------------------------------",
  "-- Alex wrapper code.",
  "-- A modified \"posn\" wrapper.",
  "-------------------------------------------------------------------",
  "",
  "data Posn = Pn !Int !Int !Int",
  "      deriving (Eq, Show,Ord)",
  "",
  "alexStartPos :: Posn",
  "alexStartPos = Pn 0 1 1",
  "",
  "alexMove :: Posn -> Char -> Posn",
  "alexMove (Pn a l c) '\\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)",
  "alexMove (Pn a l c) '\\n' = Pn (a+1) (l+1)   1",
  "alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)",
  "",
  "type AlexInput = (Posn,     -- current position,",
  "                  Char,     -- previous char",
  "                  "++stringType++")   -- current input string",
  "",
  "tokens :: "++stringType++" -> [Token]",
  "tokens str = go (alexStartPos, '\\n', str)",
  "    where",
  "      go :: AlexInput -> [Token]",
  "      go inp@(pos, _, str) =",
  "               case alexScan inp 0 of",
  "                AlexEOF                -> []",
  "                AlexError (pos, _, _)  -> [Err pos]",
  "                AlexSkip  inp' len     -> go inp'",
  "                AlexToken inp' len act -> act pos ("++stringTake++" len str) : (go inp')",
  "",
  "alexGetChar :: AlexInput -> Maybe (Char,AlexInput)",
  "alexGetChar (p, _, s) =",
  "  case "++stringUncons++" s of",
  "    "++stringNilP++"  -> Nothing",
  "    "++stringConsP++" ->",
  "             let p' = alexMove p c",
  "              in p' `seq` Just (c, (p', c, s))",
  "",
  "alexInputPrevChar :: AlexInput -> Char",
  "alexInputPrevChar (p, c, s) = c",
  "}"
  ]
 where
   (stringType,stringTake,stringUncons,stringPack,stringUnpack,stringNilP,stringConsP)
       | byteStrings = ("BS.ByteString", "BS.take", "BS.uncons", "BS.pack", "BS.unpack", "Nothing", "Just (c,s)")
       | otherwise   = ("String",        "take",    "",          "id",      "id",        "[]",      "(c:s)"     )

   ifC cat s = if isUsedCat cf cat then s else ""
   lexComments ([],[])           = []
   lexComments (xs,s1:ys) = '\"' : s1 ++ "\"" ++ " [.]* ; -- Toss single line comments\n" ++ lexComments (xs, ys)
   lexComments (([l1,l2],[r1,r2]):xs,[]) = concat
                                        [
                                        '\"':l1:l2:"\" ([$u # \\", -- FIXME quotes or escape?
                                        r1:"] | \\",
                                        r1:"+ [$u # [\\",
                                        r1:" \\",
                                        r2:"]])* (\"",
                                        r1:"\")+ \"",
                                        r2:"\" ; \n",
                                        lexComments (xs, [])
                                        ]
   lexComments (_:xs,[]) = lexComments (xs,[])
---   lexComments (xs,(_:ys)) = lexComments (xs,ys)

   -- tokens consisting of special symbols
   pTSpec [] = ""
   pTSpec _ = "@rsyms { tok (\\p s -> PT p (eitherResIdent (TV . share) s)) }"

   userDefTokenTypes = unlines
     [printRegAlex exp ++
      " { tok (\\p s -> PT p (eitherResIdent (T_"  ++ show name ++ " . share) s)) }"
      | (name,exp) <- tokenPragmas cf]
   userDefTokenConstrs = unlines
     [" | T_" ++ name ++ " !"++stringType | name <- tokenNames cf]
   userDefTokenPrint = unlines
     ["  PT _ (T_" ++ name ++ " s) -> s" | name <- tokenNames cf]

   ident =
     "$l $i*   { tok (\\p s -> PT p (eitherResIdent (TV . share) s)) }"
     --ifC "Ident"  "<ident>   ::= ^l ^i*   { ident  p = PT p . eitherResIdent TV }"

   resws = reservedWords cf ++ symbols cf


data BTree = N | B String Int BTree BTree

instance Show BTree where
    showsPrec _ N = showString "N"
    showsPrec n (B s k l r) = wrap (showString "b " . shows s  . showChar ' '. shows k  . showChar ' '
                                    . showsPrec 1 l . showChar ' '
                                    . showsPrec 1 r)
        where wrap f = if n > 0 then showChar '(' . f . showChar ')' else f

sorted2tree :: [(String,Int)] -> BTree
sorted2tree [] = N
sorted2tree xs = B x n (sorted2tree t1) (sorted2tree t2) where
  (t1,(x,n):t2) = splitAt (length xs `div` 2) xs


-------------------------------------------------------------------
-- Inlined version of @BNFC.Backend.Haskell.RegToAlex@.
-- Syntax has changed...
-------------------------------------------------------------------

-- modified from pretty-printer generated by the BNF converter

-- the top-level printing method
printRegAlex :: Reg -> String
printRegAlex = render . prt 0

-- you may want to change render and parenth

render :: [String] -> String
render = rend 0
    where rend :: Int -> [String] -> String
          rend i ss = case ss of
                        "["      :ts -> cons "["  $ rend i ts
                        "("      :ts -> cons "("  $ rend i ts
                        t  : "," :ts -> cons t    $ space "," $ rend i ts
                        t  : ")" :ts -> cons t    $ cons ")"  $ rend i ts
                        t  : "]" :ts -> cons t    $ cons "]"  $ rend i ts
                        t        :ts -> space t   $ rend i ts
                        _            -> ""

          cons s t  = s ++ t
          space t s = if null s then t else t ++ " " ++ s

parenth :: [String] -> [String]
parenth ss = ["("] ++ ss ++ [")"]

-- the printer class does the job
class Print a where
  prt :: Int -> a -> [String]
  prtList :: [a] -> [String]
  prtList = concatMap (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ c = if isAlphaNum c then [[c]] else ['\\':[c]]
  prtList = map (concat . prt 0)

prPrec :: Int -> Int -> [String] -> [String]
prPrec i j = if j<i then parenth else id

instance Print Ident where
  prt _ (Ident i) = [i]

instance Print Reg where
  prt i e = case e of
   RSeq reg0 reg -> prPrec i 2 (prt 2 reg0 ++ prt 3 reg)
   RAlt reg0 reg -> prPrec i 1 (concat [prt 1 reg0 , ["|"] , prt 2 reg])
   RMinus reg0 reg -> prPrec i 1 (concat [prt 2 reg0 , ["#"] , prt 2 reg])
   RStar reg -> prPrec i 3 (prt 3 reg ++ ["*"])
   RPlus reg -> prPrec i 3 (prt 3 reg ++ ["+"])
   ROpt reg  -> prPrec i 3 (prt 3 reg ++ ["?"])
   REps  -> prPrec i 3 ["()"]
   RChar c -> prPrec i 3 (prt 0 c)
   RAlts str -> prPrec i 3 (concat [["["],prt 0 str,["]"]])
   RSeqs str -> prPrec i 2 (concatMap (prt 0) str)
   RDigit  -> prPrec i 3 ["$d"]
   RLetter  -> prPrec i 3 ["$l"]
   RUpper  -> prPrec i 3 ["$c"]
   RLower  -> prPrec i 3 ["$s"]
   RAny  -> prPrec i 3 ["$u"]
