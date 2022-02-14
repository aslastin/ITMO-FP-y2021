module HW3.Pretty
  ( addColor
  , prettyValue
  ) where

import Prettyprinter (Doc, Pretty, annotate, colon, comma, encloseSep, fillSep, lbrace, lbracket,
                      list, lparen, pretty, rbrace, rbracket, rparen, space, surround, viaShow,
                      (<+>))
import Prettyprinter.Render.Terminal.Internal (AnsiStyle,
                                               Color (Blue, Cyan, Green, Magenta, White, Yellow),
                                               color, colorDull, italicized)

import HW3.Base (HiAction (..), HiValue (..), getFunName)

import Numeric (showHex)

import Data.Foldable (fold, toList)
import Data.Function (on)
import Data.Maybe (isNothing)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)

import GHC.Real (Ratio ((:%)))

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Time.Clock (UTCTime)


-- | Almost all HiValues have color to emphasize their type

prettyValue :: HiValue -> Doc AnsiStyle

prettyValue (HiValueNumber n) = prettyNumber n

prettyValue (HiValueFunction fun) = prettyFunName $ getFunName fun

prettyValue (HiValueBool bool) = prettyBool bool

prettyValue HiValueNull = prettyNull

prettyValue (HiValueString str) = prettyString str

prettyValue (HiValueList sq) = prettySeq sq

prettyValue (HiValueBytes bs) = prettyBytes bs

prettyValue (HiValueAction action) = case action of
  (HiActionRead fp)    -> prettyAction "read" [prettyString fp]
  (HiActionWrite fp s) -> prettyAction "write" [prettyString fp, prettyBytes s]
  (HiActionMkDir fp)   -> prettyAction "mkdir" [prettyString fp]
  (HiActionChDir fp)   -> prettyAction "cd" [prettyString fp]
  HiActionCwd          -> prettyFunName "cwd"
  HiActionNow          -> prettyFunName "now"
  (HiActionRand l r)   -> prettyAction "rand" [colorNumber l, colorNumber r]
  (HiActionEcho s)     -> prettyAction "echo" [prettyString s]

prettyValue (HiValueTime time) = prettyTime time

prettyValue (HiValueDict d) = encloseSep lbrace' rbrace' comma' $ map (uncurry f) $ Map.toAscList d
  where
    lbrace' = lbrace <> space
    rbrace' = space <> rbrace
    comma' = comma <> space
    f = (\a b -> a <+> addColor White colon <+> b) `on` prettyValue


-- | Every instance of HIValue i.e. HiValueNumber, HiValueString ...
-- has it's method which colors HiValue's inner object.
--
-- It's handy because we can easily reuse all of them.
prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber (n@(p :% q))
  | q == 1 = colorNumber p
  | isNothing mbRepetendIx = colorNumber $ formatScientific Fixed Nothing s
  | abs p < q = colorNumber p <> pretty "/" <> colorNumber q
  | otherwise =  colorNumber a <+> pretty sign <+> colorNumber (abs b) <> pretty "/" <> colorNumber q
    where
      (s, mbRepetendIx) = fromRationalRepetendUnlimited n
      (a, b) = quotRem p q
      sign = if b > 0 then "+" else "-"


-- | Numbers are colored with @'Cyan'@
colorNumber :: (Pretty a) => a -> Doc AnsiStyle
colorNumber = prettyColored Cyan


-- | Functions are @'Yellow'@
prettyFunName :: String -> Doc AnsiStyle
prettyFunName = prettyColored Yellow


-- | Strings as always @'Green'@
prettyString :: (Show a) => a -> Doc AnsiStyle
prettyString = addColor Green . viaShow


-- | Colors aren't unlimited so I'm changing @'Cyan'@'s brightness
prettyBool :: Bool -> Doc AnsiStyle
prettyBool bool = applyStyles [colorDull Cyan, italicized] $ if bool then "true" else "false"


-- | @'Blue'@
prettyNull :: Doc AnsiStyle
prettyNull = prettyColored Blue "null"


-- | I don't fully color container types like lists, bytestring or dictionary.
-- However, I don't forget to color their elements
prettySeq :: Seq.Seq HiValue -> Doc AnsiStyle
prettySeq = list . toList . fmap prettyValue


-- | Hashes are emphasized with @'Magenta'@ color
prettyBytes :: BS.ByteString -> Doc AnsiStyle
prettyBytes bs = surround (fillSep bytes) (lbracket <> hash <> space) (space <> hash <> rbracket)
  where
    bytes =  map (pretty . adder . flip showHex "") $ BS.unpack bs
    adder a = if length a < 2 then '0' : a else a
    hash = prettyColored Magenta "#"


prettyTime :: UTCTime -> Doc AnsiStyle
prettyTime time = prettyAction "parse-time" [prettyString $ show time]


-- | Action is colored as normal function call that's why action's name has function's color
prettyAction :: String -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyAction actionName args = prettyFunName actionName <> encloseSep lparen rparen (comma <> space) args


-- | Base coloring and styling methods


prettyColored :: (Pretty a) => Color -> a -> Doc AnsiStyle
prettyColored c = addColor c . pretty


addColor :: Color -> Doc AnsiStyle -> Doc AnsiStyle
addColor c = annotate (color c)


applyStyles :: (Pretty a) => [AnsiStyle] -> a -> Doc AnsiStyle
applyStyles styles =  annotate (fold styles) . pretty
