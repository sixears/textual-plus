{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module TextualPlus
  ( PrintOut( toP ), parseTextual )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Monad  ( return )
import Data.Function  ( ($) )
import Data.String    ( String )
import Data.Typeable  ( Typeable, typeOf )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

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

-- that's all, folks! ----------------------------------------------------------
