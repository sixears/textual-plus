{-# LANGUAGE DeriveGeneric #-}

{-| Errors incurred while parsing a `Data.Textual.Textual` value -}

module TextualPlus.Error.TextualParseError
  ( AsTextualParseError( _TextualParseError ), TextualParseError
  , throwAsTextualParseError, tparseToME, tparseToME'
  )
where

-- Pragmata ------------------------------------------------

import Base0

-- base --------------------------------

import GHC.Generics  ( Generic )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- more-unicode ------------------------

import Data.MoreUnicode.String  ( 𝕊 )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

{-| Error while trying to parse a `Data.Textual.Textual` -}
data TextualParseError =
  TextualParseError { _es ∷ 𝕊, _ess ∷ [𝕊], _callstack ∷ CallStack }
  deriving (Generic, Show)

----------

instance Eq TextualParseError where
  (TextualParseError a as _) == (TextualParseError b bs _) = a == b ∧ as ≡ bs

----------

instance HasCallstack TextualParseError where
  callstack = lens _callstack (\ tpe cs → tpe { _callstack = cs })

----------

instance NFData TextualParseError

----------

instance Printable TextualParseError where
  print (TextualParseError es ess _) =
    P.text $ [fmt|failed to parse: [%L] %s|] ess es

------------------------------------------------------------

{-| Error type that may include a `TextualParseError` -}
class AsTextualParseError ε where
  _TextualParseError ∷ Prism' ε TextualParseError

----------

instance AsTextualParseError TextualParseError where
  _TextualParseError = id

--------------------

asTextualParseError ∷ (HasCallStack, AsTextualParseError ε) ⇒
                      𝕊 → [𝕊] → ε
asTextualParseError es ess =
  _TextualParseError # TextualParseError es ess callStack

----------

{-| throw an `AsTextualParseError` -}
throwAsTextualParseError ∷ (AsTextualParseError ε, MonadError ε η) ⇒
                           𝕊 → [𝕊] → η α
throwAsTextualParseError es = throwError ∘ asTextualParseError es

------------------------------------------------------------

{-| convert a `Parsed` value to an `AsTextualParseError` in a `MonadError`
    context -}
tparseToME ∷ ∀ ε α η . (AsTextualParseError ε, MonadError ε η) ⇒ Parsed α → η α
tparseToME (Parsed a)       = return a
tparseToME (Malformed ss s) = throwAsTextualParseError s ss

--------------------

{-| convert a `Parsed` value to either an @α@ or a `TextualParseError` -}
tparseToME' ∷ Parsed α → Either TextualParseError α
tparseToME' = tparseToME

-- that's all, folks! ----------------------------------------------------------
