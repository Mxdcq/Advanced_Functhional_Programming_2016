-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParBNF where
import AbsBNF
import LexBNF
import ErrM

}

%name pLGrammar LGrammar
%name pLDef LDef
%name pListLDef ListLDef
%name pGrammar Grammar
%name pListDef ListDef
%name pListItem ListItem
%name pDef Def
%name pItem Item
%name pCat Cat
%name pLabel Label
%name pLabelId LabelId
%name pProfItem ProfItem
%name pIntList IntList
%name pListInteger ListInteger
%name pListIntList ListIntList
%name pListProfItem ListProfItem
%name pSeparation Separation
%name pArg Arg
%name pListArg ListArg
%name pExp Exp
%name pExp1 Exp1
%name pExp2 Exp2
%name pListExp2 ListExp2
%name pListExp ListExp
%name pListString ListString
%name pListRHS ListRHS
%name pRHS RHS
%name pMinimumSize MinimumSize
%name pReg2 Reg2
%name pReg1 Reg1
%name pReg3 Reg3
%name pReg Reg
%name pListIdent ListIdent

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  '*' { PT _ (TS _ 3) }
  '+' { PT _ (TS _ 4) }
  ',' { PT _ (TS _ 5) }
  '-' { PT _ (TS _ 6) }
  '.' { PT _ (TS _ 7) }
  ':' { PT _ (TS _ 8) }
  '::=' { PT _ (TS _ 9) }
  ';' { PT _ (TS _ 10) }
  '=' { PT _ (TS _ 11) }
  '?' { PT _ (TS _ 12) }
  '[' { PT _ (TS _ 13) }
  ']' { PT _ (TS _ 14) }
  '_' { PT _ (TS _ 15) }
  'char' { PT _ (TS _ 16) }
  'coercions' { PT _ (TS _ 17) }
  'comment' { PT _ (TS _ 18) }
  'define' { PT _ (TS _ 19) }
  'delimiters' { PT _ (TS _ 20) }
  'digit' { PT _ (TS _ 21) }
  'entrypoints' { PT _ (TS _ 22) }
  'eps' { PT _ (TS _ 23) }
  'internal' { PT _ (TS _ 24) }
  'layout' { PT _ (TS _ 25) }
  'letter' { PT _ (TS _ 26) }
  'lower' { PT _ (TS _ 27) }
  'nonempty' { PT _ (TS _ 28) }
  'position' { PT _ (TS _ 29) }
  'rules' { PT _ (TS _ 30) }
  'separator' { PT _ (TS _ 31) }
  'stop' { PT _ (TS _ 32) }
  'terminator' { PT _ (TS _ 33) }
  'token' { PT _ (TS _ 34) }
  'toplevel' { PT _ (TS _ 35) }
  'upper' { PT _ (TS _ 36) }
  'views' { PT _ (TS _ 37) }
  '{' { PT _ (TS _ 38) }
  '|' { PT _ (TS _ 39) }
  '}' { PT _ (TS _ 40) }

L_quoted { PT _ (TL $$) }
L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_charac { PT _ (TC $$) }
L_doubl  { PT _ (TD $$) }


%%

String  :: { String }  : L_quoted {  $1 }
Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Char    :: { Char }    : L_charac { (read ( $1)) :: Char }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }

LGrammar :: { LGrammar }
LGrammar : ListLDef { LGr $1 } 


LDef :: { LDef }
LDef : Def { DefAll $1 } 
  | ListIdent ':' Def { DefSome $1 $3 }
  | 'views' ListIdent { LDefView $2 }


ListLDef :: { [LDef] }
ListLDef : {- empty -} { [] } 
  | LDef { (:[]) $1 }
  | LDef ';' ListLDef { (:) $1 $3 }


Grammar :: { Grammar }
Grammar : ListDef { Grammar $1 } 


ListDef :: { [Def] }
ListDef : {- empty -} { [] } 
  | Def { (:[]) $1 }
  | Def ';' ListDef { (:) $1 $3 }


ListItem :: { [Item] }
ListItem : {- empty -} { [] } 
  | ListItem Item { flip (:) $1 $2 }


Def :: { Def }
Def : Label '.' Cat '::=' ListItem { Rule $1 $3 (reverse $5) } 
  | 'comment' String { Comment $2 }
  | 'comment' String String { Comments $2 $3 }
  | 'internal' Label '.' Cat '::=' ListItem { Internal $2 $4 (reverse $6) }
  | 'token' Ident Reg { Token $2 $3 }
  | 'position' 'token' Ident Reg { PosToken $3 $4 }
  | 'entrypoints' ListIdent { Entryp $2 }
  | 'separator' MinimumSize Cat String { Separator $2 $3 $4 }
  | 'terminator' MinimumSize Cat String { Terminator $2 $3 $4 }
  | 'delimiters' Cat String String Separation MinimumSize { Delimiters $2 $3 $4 $5 $6 }
  | 'coercions' Ident Integer { Coercions $2 $3 }
  | 'rules' Ident '::=' ListRHS { Rules $2 $4 }
  | 'define' Ident ListArg '=' Exp { Function $2 (reverse $3) $5 }
  | 'layout' ListString { Layout $2 }
  | 'layout' 'stop' ListString { LayoutStop $3 }
  | 'layout' 'toplevel' { LayoutTop }


Item :: { Item }
Item : String { Terminal $1 } 
  | Cat { NTerminal $1 }


Cat :: { Cat }
Cat : '[' Cat ']' { ListCat $2 } 
  | Ident { IdCat $1 }


Label :: { Label }
Label : LabelId { LabNoP $1 } 
  | LabelId ListProfItem { LabP $1 $2 }
  | LabelId LabelId ListProfItem { LabPF $1 $2 $3 }
  | LabelId LabelId { LabF $1 $2 }


LabelId :: { LabelId }
LabelId : Ident { Id $1 } 
  | '_' { Wild }
  | '[' ']' { ListE }
  | '(' ':' ')' { ListCons }
  | '(' ':' '[' ']' ')' { ListOne }


ProfItem :: { ProfItem }
ProfItem : '(' '[' ListIntList ']' ',' '[' ListInteger ']' ')' { ProfIt $3 $7 } 


IntList :: { IntList }
IntList : '[' ListInteger ']' { Ints $2 } 


ListInteger :: { [Integer] }
ListInteger : {- empty -} { [] } 
  | Integer { (:[]) $1 }
  | Integer ',' ListInteger { (:) $1 $3 }


ListIntList :: { [IntList] }
ListIntList : {- empty -} { [] } 
  | IntList { (:[]) $1 }
  | IntList ',' ListIntList { (:) $1 $3 }


ListProfItem :: { [ProfItem] }
ListProfItem : ProfItem { (:[]) $1 } 
  | ProfItem ListProfItem { (:) $1 $2 }


Separation :: { Separation }
Separation : {- empty -} { SepNone } 
  | 'terminator' String { SepTerm $2 }
  | 'separator' String { SepSepar $2 }


Arg :: { Arg }
Arg : Ident { Arg $1 } 


ListArg :: { [Arg] }
ListArg : {- empty -} { [] } 
  | ListArg Arg { flip (:) $1 $2 }


Exp :: { Exp }
Exp : Exp1 ':' Exp { Cons $1 $3 } 
  | Exp1 { $1 }


Exp1 :: { Exp }
Exp1 : Ident ListExp2 { App $1 $2 } 
  | Exp2 { $1 }


Exp2 :: { Exp }
Exp2 : Ident { Var $1 } 
  | Integer { LitInt $1 }
  | Char { LitChar $1 }
  | String { LitString $1 }
  | Double { LitDouble $1 }
  | '[' ListExp ']' { List $2 }
  | '(' Exp ')' { $2 }


ListExp2 :: { [Exp] }
ListExp2 : Exp2 { (:[]) $1 } 
  | Exp2 ListExp2 { (:) $1 $2 }


ListExp :: { [Exp] }
ListExp : {- empty -} { [] } 
  | Exp { (:[]) $1 }
  | Exp ',' ListExp { (:) $1 $3 }


ListString :: { [String] }
ListString : String { (:[]) $1 } 
  | String ',' ListString { (:) $1 $3 }


ListRHS :: { [RHS] }
ListRHS : RHS { (:[]) $1 } 
  | RHS '|' ListRHS { (:) $1 $3 }


RHS :: { RHS }
RHS : ListItem { RHS (reverse $1) } 


MinimumSize :: { MinimumSize }
MinimumSize : 'nonempty' { MNonempty } 
  | {- empty -} { MEmpty }


Reg2 :: { Reg }
Reg2 : Reg2 Reg3 { RSeq $1 $2 } 
  | Reg3 { $1 }


Reg1 :: { Reg }
Reg1 : Reg1 '|' Reg2 { RAlt $1 $3 } 
  | Reg2 '-' Reg2 { RMinus $1 $3 }
  | Reg2 { $1 }


Reg3 :: { Reg }
Reg3 : Reg3 '*' { RStar $1 } 
  | Reg3 '+' { RPlus $1 }
  | Reg3 '?' { ROpt $1 }
  | 'eps' { REps }
  | Char { RChar $1 }
  | '[' String ']' { RAlts $2 }
  | '{' String '}' { RSeqs $2 }
  | 'digit' { RDigit }
  | 'letter' { RLetter }
  | 'upper' { RUpper }
  | 'lower' { RLower }
  | 'char' { RAny }
  | '(' Reg ')' { $2 }


Reg :: { Reg }
Reg : Reg1 { $1 } 


ListIdent :: { [Ident] }
ListIdent : Ident { (:[]) $1 } 
  | Ident ',' ListIdent { (:) $1 $3 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

