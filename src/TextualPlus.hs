module TextualPlus
  ( PrintOut( toP ), parseTextual
  , __ERR__, __error__
  , bracket, bracket', b, b'
  , encompass, encompass'
  , guillemet, guillemet', g, g'
  , parenthesize, parenthesize', p, p'
  , quote, quote', q, q'
  , qquote, qquote', qq, qq'
  , surround, surround'
  )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Monad  ( return )
import Data.Function  ( ($) )
import Data.String    ( String )
import Data.Typeable  ( Typeable, typeOf )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ), Printable, Textual
                     , parseText, toText, toString )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text     ( Text )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

--------------------------------------------------------------------------------

class PrintOut σ where
  toP ∷ Printable ρ ⇒ ρ → σ

instance PrintOut Text where
  toP = toText

instance PrintOut String where
  toP = toString

{- | Parse a printable value, give user-friendly error messages.
     This is mostly an adapter from `Printable` to `Either`; to work with, e.g.,
     `Options.Applicative.eitherReader`.
 -}
parseTextual ∷ ∀ β ε α η .
      (Textual β, PrintOut ε, Printable α, Typeable β, MonadError ε η) ⇒
      α → η β
parseTextual (toText → z) =
  let fromParsed (Parsed a)      = a
      -- this function exists solely to provide a hypothetical value to reflect
      -- on
      fromParsed (Malformed _ _) = error "this should never be evaluated"
      parsedZ                    = parseText z
      typ                        = typeOf $ fromParsed parsedZ
   in case parsedZ of
        Parsed a       → return a
        Malformed [] x → throwError ∘ toP $
                           [fmtT|failed to parse '%t' as '%w': %s|] z typ x
        Malformed xs x → let msg = [fmtT|failed to parse '%t' as '%w': [%L] %s|]
                                   z typ xs x
                          in throwError (toP msg)

{- | throw an error with some printable -}
__ERR__ ∷ Printable ρ ⇒ ρ → α
__ERR__ s = error $ toString s

{- | throw an error -}
__error__ ∷ Text → α
__error__ = __ERR__

{- | adorn text with a prefix and a suffix -}
encompass ∷ Printable ρ ⇒ Text → Text → ρ → Text
encompass pfx sfx t = pfx ⊕ toText t ⊕ sfx

encompass' ∷ Text → Text → Text → Text
encompass' = encompass

{- | adorn text with (the same) prefix suffix -}
surround ∷ Printable ρ ⇒ Text → ρ → Text
surround x = encompass x x

surround' ∷ Text → Text → Text
surround' = surround

{- | surround text with (single) quotes -}
quote ∷ Printable ρ ⇒ ρ -> Text
quote = surround "'"

quote' ∷ Text -> Text
quote' = quote

{- | short alias for `quote` -}
q ∷ Printable ρ ⇒ ρ -> Text
q = quote

q' ∷ Text -> Text
q' = quote'

{- | surround text with (double) quotes -}
qquote ∷ Printable ρ ⇒ ρ -> Text
qquote = surround "\""

qquote' ∷ Text -> Text
qquote' = qquote

{- | short alias for `qquote` -}
qq ∷ Printable ρ ⇒ ρ -> Text
qq = qquote

qq' ∷ Text -> Text
qq' = qquote'

{- | surround text with parentheses -}
parenthesize ∷ Printable ρ ⇒ ρ → Text
parenthesize = encompass "(" ")"

parenthesize' ∷ Text → Text
parenthesize' = parenthesize

{- | short alias for `parenthesize` -}
p ∷ Printable ρ ⇒ ρ -> Text
p = parenthesize

p' ∷ Text -> Text
p' = parenthesize'

{- | surround text with brackets -}
bracket ∷ Printable ρ ⇒ ρ → Text
bracket = encompass "[" "]"

bracket' ∷ Text → Text
bracket' = bracket

{- | short alias for `bracket` -}
b ∷ Printable ρ ⇒ ρ -> Text
b = bracket

b' ∷ Text -> Text
b' = bracket'

{- | surround text with guillemets -}
guillemet ∷ Printable ρ ⇒ ρ → Text
guillemet = encompass "«" "»"

guillemet' ∷ Text → Text
guillemet' = guillemet

{- | short alias for `guillemet` -}
g ∷ Printable ρ ⇒ ρ -> Text
g = guillemet

g' ∷ Text -> Text
g' = guillemet'

-- that's all, folks! ----------------------------------------------------------
