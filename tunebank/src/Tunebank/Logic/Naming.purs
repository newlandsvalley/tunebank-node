module Tunebank.Logic.Naming 
  (safeFileName) where

import Prelude


import Data.Array (filter)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.CodePoint.Unicode (isAsciiLower, isAsciiUpper, isDecDigit, isSpace)
import Data.String.CodePoints (CodePoint, codePointFromChar, fromCodePointArray, toCodePointArray)


-- | Modify a tune title in order to produce a string which is suitable for use as a file name.
-- | We restrict ourselves to ASCII characters, numbers, spaces and a hyphen.
-- | Some Swedish and Norwegian non-ASCII vowels are representd by their nearest ASCII equivalent
-- | but other non-ASCII characters are dropped.
safeFileName :: String -> String 
safeFileName = 
   cleanNordicChars 
     >>> toCodePointArray 
     >>> filter (isAsciiLower || isAsciiUpper || isDecDigit || isSpace || isHyphen) 
     >>> fromCodePointArray

   where
   isHyphen :: CodePoint -> Boolean
   isHyphen c = c == codePointFromChar '-'


cleanNordicChars :: String -> String 
cleanNordicChars = 
  replaceå  
    >>> replaceÅ
    >>> replaceä 
    >>> replaceÄ 
    >>> replaceö 
    >>> replaceÖ
    >>> replaceø
    >>> replaceØ
    >>> replaceæ
    >>> replaceÆ

-- Swedish and Norwegian
replaceå :: String -> String 
replaceå = 
  replaceAll (Pattern "å") (Replacement "a")

replaceÅ :: String -> String 
replaceÅ = 
  replaceAll (Pattern "Å") (Replacement "A")

-- Swedish
replaceä :: String -> String 
replaceä = 
  replaceAll (Pattern "ä") (Replacement "a")

replaceÄ :: String -> String 
replaceÄ = 
  replaceAll (Pattern "Ä") (Replacement "A")

replaceö :: String -> String 
replaceö = 
  replaceAll (Pattern "ö") (Replacement "o")

replaceÖ :: String -> String 
replaceÖ = 
  replaceAll (Pattern "Ö") (Replacement "O")

--- Norwegian
replaceø :: String -> String 
replaceø = 
  replaceAll (Pattern "ø") (Replacement "o")

replaceØ :: String -> String 
replaceØ = 
  replaceAll (Pattern "Ø") (Replacement "O")

replaceæ :: String -> String 
replaceæ = 
  replaceAll (Pattern "æ") (Replacement "a")

replaceÆ :: String -> String 
replaceÆ = 
  replaceAll (Pattern "Æ") (Replacement "A")


