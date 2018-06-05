{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CalculiTester.Printer where

import Control.Lens
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Tree
import CalculiTester.TypeDef

space = showChar ' '

--compose printings with spaces between
thread :: (a → ShowS) → [a] → ShowS
thread f [] = id
thread f [x] = f x
thread f (x:xs) = f x . space . thread f xs

--show function for Text
showT = showString . T.unpack

--printic function for Rec
printRec :: Fixity → Rec Text → ShowS
printRec fixity = printRecPrec fixity (Just 0)

--print a Rec in a precedence environment
--Nothing signifies the infinite precedence
--in effect for an argument of a function
printRecPrec :: Fixity → Maybe Int → Rec Text → ShowS
--variable and symbols are just printed
printRecPrec fixity _ (Var x) = showT x
printRecPrec fixity _ (Atom x []) = showT x
--print complex expression
--if a complex expression is argument of a function,
--it should have parentheses
printRecPrec fixity Nothing expr =
  --lists have a special printing function
  if isListRec expr
  then printListRec fixity expr
  --prints parentheses and calls the function
  --for printing complex expressions
  --inside the parentheses the precedence is 0
  else showParen True (printRecPrecInner fixity 0 expr)
printRecPrec fixity (Just n) expr =
  --lists have a special printing function
  if isListRec expr
  then printListRec fixity expr
  --calls function for printing complex expressions
  else printRecPrecInner fixity n expr

--prints a complex expression
--separated because of complicated logic of putting parentheses
--with operators
printRecPrecInner :: Fixity → Int → Rec Text → ShowS
printRecPrecInner fixity d (Atom constr args@(arg:args')) =
  case M.lookup constr fixity of
    --a function doesn't need parenteses
    Nothing → showT constr . space .
      --each argument is inside a function argument precedence
      thread (printRecPrec fixity Nothing) args
    --infix operators needs parentheses if their
    --associativity is strictly lower than the environment
    Just (Infix ALeft n) → showParen (n < d) $
      --left operant of left associative operator is
      --inside the same precedence
      printRecPrec fixity (Just n) arg . space .
      --the operator is between the operand
      showT constr . space .
      --right operator is inside incremented precedence
      thread (printRecPrec fixity (Just (n+1))) args'
    --opposite situation to the left associtive operator
    Just (Infix ARight n) → showParen (n < d) $
      printRecPrec fixity (Just (n+1)) arg . space .
      showT constr . space .
      thread (printRecPrec fixity (Just n)) args'
    --prefix and postfix operators show their arguments in
    --parenthesis because showing them properly is too complicated
    Just (Prefix) → showT constr . printRecPrec fixity Nothing arg
    Just (Postfix) → printRecPrec fixity Nothing arg . showT constr

--list is a record with head "#", such so that the second operator is
--a list, or an empty list
isListRec :: Rec Text → Bool
isListRec (Atom "[]" []) = True
isListRec (Atom "#" (_ : arg : args)) = isListRec arg
isListRec _ = False

--list are printed inside square brackets, with each argument is
--in zero precedence environment
printListRec :: Fixity → Rec Text → ShowS
printListRec fixity expr =
  showChar '[' . go fixity expr where
  go fixity (Atom "#" (hd:Atom "[]" []:_)) =
    printRecPrec fixity (Just 0) hd . showChar ']'
  go fixity (Atom "#" (hd:tl:_)) =
    printRecPrec fixity (Just 0) hd . showString "; " .
    go fixity tl

type Width = Int
type Height = Int

--represents Height of lines, each with the length Width
type TextRectangle = (Width, Height, [ShowS])

--constants for sepating showed subtrees
premiseSepLen = 2
premiseSep = whiteSpace premiseSepLen

--like replicate but with ShowS
showS_n :: Char → Int → ShowS
showS_n _ 0 = id
showS_n c n = if n < 0
              then error "showS_n fail"
              else showChar c . showS_n c (n - 1)

whiteSpace = showS_n ' '

--like intersperse but with ShowS
intersperse :: ShowS → [ShowS] → ShowS
intersperse sep = \case
  [] → id
  ts → foldr1 (\f g → f . sep . g) ts

--makes a list of rectangles into one rectangles,
--gluing them horizontally and aligning to the bottom
spliceRectangles :: [TextRectangle] → TextRectangle
--empty list give empty rectangle
spliceRectangles [] = (0,0,[])
spliceRectangles rs =
  let
    --extracts widths, heights and texts
    ws = map (^. _1) rs
    hs = map (^. _2) rs
    ts = map (^. _3) rs
    --the height of the splice is the maximum of the heigths
    h = maximum hs
    --add white lines to the top of each rectangle
    --to make them all have height h
    tsEven = zipWith3 (\w h' t → replicate (h - h') (whiteSpace w) ++ t)
             ws hs ts
    --concatenate texts by line with separations
    t = map (intersperse premiseSep) $ transpose tsEven
    --calculate the full width
    --[x,y,..z] → x + premiseSep + y + premiseSep + .. + premiseSep + z
    w = case ws of
      [] → 0
      ws → foldr1 ((.) (+ premiseSepLen) . (+)) ws
  in (w, h, t)

--extending string of length l to length n with whitespace
padString :: Int → Int → ShowS → ShowS
padString n l s =
  let
    leftPad = (n - l) `div` 2
    rightPad = n - l - leftPad
  in whiteSpace leftPad . s . whiteSpace rightPad

--convert a proof into a rectangle of text
treeToText :: Fixity → Tree (RuleName, Rec Text) → TextRectangle
treeToText fixity (Node (rule, expr) subT) =
  let
    --make a rectangle from subtrees
    (w1, h1, body') = spliceRectangles $ map (treeToText fixity) subT
    --make a string from the head
    concl' = printRec fixity expr
    --calculatate the length of the head
    l = length $ concl' ""
    --calculate the lenght of the name of the rule
    ruleLength = fromIntegral (T.length rule)
    --function to generate whitespace over and under the rule name
    padRight = (. whiteSpace ruleLength)
    --decide the total width and what needs to be padded
    --both concl and body should have width w
    (concl, w, body) = case compare w1 l of
      LT → (concl', l, fmap (padString l w1) body')
      EQ → (concl', w1, body')
      GT → (padString w1 l concl', w1, body')
  in
    --add the name of the rule to the right, add necessary whitespace,
    --adjust width and height
    (w + fromIntegral (T.length rule), h1 + 2,
      map padRight body ++ [showS_n '─' w . showString (T.unpack rule), padRight concl])

--makes the rectangle, and shows it
printTree :: Fixity → Tree (RuleName, Rec Text) → ShowS
printTree fixity tree =
  let
    (_,_, text) = treeToText fixity tree
  in foldr (.) id (fmap (. showChar '\n') text)
