{-# LANGUAGE UnicodeSyntax #-}
{-| tools for working with Text, including parsing & human-printing -}
module TextualPlus
  ( ParseableInput(tparse, tparse')
  , Parser
  , PrintOut(toP)
  , TextualPlus(..)
  , __ERR__
  , __error__
  , b
  , b'
  , bracket
  , bracket'
  , checkT
  , encompass
  , encompass'
  , g
  , g'
  , guillemet
  , guillemet'
  , p
  , p'
  , parenthesize
  , parenthesize'
  , parse
  , parseLazyText
  , parseString
  , parseT
  , parseText
  , parseTextM
  , parseTextual
  , parseUtf8
  , propInvertibleString
  , propInvertibleText
  , propInvertibleUtf8
  , q
  , q'
  , qq
  , qq'
  , qquote
  , qquote'
  , quote
  , quote'
  , surround
  , surround'
  ) where

-- Pragmata ------------------------------------------------

import Base0
import Prelude ( error )

-- base --------------------------------

import Control.Applicative ( Alternative(empty, (<|>)) )
import Control.Monad       ( MonadFail(fail) )
import Data.Functor        ( Functor )
import Data.List           ( length, reverse, stripPrefix )
import Data.Typeable       ( typeOf )
import Data.Word           ( Word )

-- bytestring --------------------------

import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BL

-- data-textual ------------------------

import Data.Textual ( Parsed(Malformed, Parsed), toUtf8 )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative ( (⋪), (⋫) )
import Data.MoreUnicode.Either      ( pattern 𝕽 )
import Data.MoreUnicode.Functor     ( (⊳) )
import Data.MoreUnicode.Maybe       ( pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.String      ( 𝕊 )
import Data.MoreUnicode.Text        ( 𝕋 )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, satisfy, string )
import Text.Parser.Combinators ( Parsing(notFollowedBy, skipMany, skipSome),
                                 eof, try, unexpected, (<?>) )

-- tasty -------------------------------

import Test.Tasty ( TestTree )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit ( testCase, (@=?) )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck ( Property, (===) )

-- text --------------------------------

import Data.Text      qualified as Text
import Data.Text.Lazy qualified as TL

import Data.Text               ( intercalate, unpack )
import Data.Text.Lazy.Encoding ( decodeUtf8 )

-- text-printer ------------------------

import Text.Printer qualified as P

-- tfmt --------------------------------

import Text.Fmt ( fmt, fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError,
                                             TextualParseError, tparseToME,
                                             tparseToME' )

--------------------------------------------------------------------------------

{-| Just as `Data.Textual.Textual`, but we add some more capabalities -}
class TextualPlus α where
  textual' ∷ (MonadFail μ, CharParsing μ) ⇒ μ α

------------------------------------------------------------

{-| types that may be printed to -}
class PrintOut σ where
  toP ∷ Printable ρ ⇒ ρ → σ

instance PrintOut 𝕋 where
  toP = toText

instance PrintOut 𝕊 where
  toP = toString

{- | Parse a printable value, give user-friendly error messages.
     This is mostly an adapter from `Printable` to `Either`; to work with, e.g.,
     `Options.Applicative.eitherReader`.
 -}
parseTextual ∷ ∀ β ε α η .
      (TextualPlus β, PrintOut ε, Printable α, Typeable β, MonadError ε η) ⇒
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
__error__ ∷ 𝕋 → α
__error__ = __ERR__

{- | adorn text with a prefix and a suffix -}
encompass ∷ Printable ρ ⇒ 𝕋 → 𝕋 → ρ → 𝕋
encompass pfx sfx t = pfx ⊕ toText t ⊕ sfx

-- | `encompass`, specialized to `𝕋`
encompass' ∷ 𝕋 → 𝕋 → 𝕋 → 𝕋
encompass' = encompass

-- | adorn text with (the same) prefix suffix
surround ∷ Printable ρ ⇒ 𝕋 → ρ → 𝕋
surround x = encompass x x

-- | `surround` specialized to 𝕋
surround' ∷ 𝕋 → 𝕋 → 𝕋
surround' = surround

-- | surround text with (single) quotes
quote ∷ Printable ρ ⇒ ρ → 𝕋
quote = surround "'"

-- | `quote` specialized to 𝕋
quote' ∷ 𝕋 → 𝕋
quote' = quote

-- | short alias for `quote`
q ∷ Printable ρ ⇒ ρ → 𝕋
q = quote

-- | short alias for `quote'`
q' ∷ 𝕋 → 𝕋
q' = quote'

-- | surround text with (double) quotes
qquote ∷ Printable ρ ⇒ ρ → 𝕋
qquote = surround "\""

-- | `qquote` specialized to 𝕋
qquote' ∷ 𝕋 → 𝕋
qquote' = qquote

{- | short alias for `qquote` -}
qq ∷ Printable ρ ⇒ ρ → 𝕋
qq = qquote

-- | short alias for `qquote'`
qq' ∷ 𝕋 → 𝕋
qq' = qquote'

{- | surround text with parentheses -}
parenthesize ∷ Printable ρ ⇒ ρ → 𝕋
parenthesize = encompass "(" ")"

-- | `parenthesize` specialized to 𝕋
parenthesize' ∷ 𝕋 → 𝕋
parenthesize' = parenthesize

{- | short alias for `parenthesize` -}
p ∷ Printable ρ ⇒ ρ → 𝕋
p = parenthesize

-- | short alias for `parenthesize'`
p' ∷ 𝕋 → 𝕋
p' = parenthesize'

{- | surround text with brackets -}
bracket ∷ Printable ρ ⇒ ρ → 𝕋
bracket = encompass "[" "]"

-- | `bracket`, specialized to `𝕋`
bracket' ∷ 𝕋 → 𝕋
bracket' = bracket

{- | short alias for `bracket` -}
b ∷ Printable ρ ⇒ ρ → 𝕋
b = bracket

-- | short alias for `bracket'`
b' ∷ 𝕋 → 𝕋
b' = bracket'

{- | surround text with guillemets -}
guillemet ∷ Printable ρ ⇒ ρ → 𝕋
guillemet = encompass "«" "»"

-- | `guillemet`, specialized to 𝕋
guillemet' ∷ 𝕋 → 𝕋
guillemet' = guillemet

{- | short alias for `guillemet` -}
g ∷ Printable ρ ⇒ ρ → 𝕋
g = guillemet

-- | short alias for `guillemet'`
g' ∷ 𝕋 → 𝕋
g' = guillemet'

----------------------------------------

{-| frankly, I forget why I re-implemented this, but I'm sure there was a good
    reason -}
data Parser α = Parser { runParser :: forall r. [𝕊] -> Word -> 𝕊 -> ([𝕊] -> Word -> 𝕊 -> α -> Parsed r) -> ([𝕊] -> Word -> 𝕊 -> 𝕊 -> Parsed r) -> Parsed r
                       }

instance Functor Parser where
  fmap f prsr = Parser $ \ ls n i c h →
               runParser prsr ls n i (\ ls' n' i' a → c ls' n' i' (f a)) h
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure a = Parser $ \ ls n i c _ → c ls n i a
  {-# INLINE pure #-}
  prsr <*> prsr' = Parser $ \ ls n i c h →
                              runParser prsr ls n i
                                        (\ ls' n' i' f →
                                           runParser prsr' ls' n' i'
                                                     (\ ls'' n'' i'' a →
                                                        c ls'' n'' i'' (f a)) h)
                                        h
  {-# INLINE (<*>) #-}

  prsr *> prsr' =
    Parser $ \ ls n i c h →
      runParser prsr ls n i (\ ls' n' i' _ → runParser prsr' ls' n' i' c h) h
  {-# INLINE (*>) #-}

  prsr <* prsr' = Parser $ \ ls n i c h →
              runParser prsr ls n i
                        (\ ls' n' i' a →
                           runParser prsr' ls' n' i'
                                     (\ ls'' n'' i'' _ → c ls'' n'' i'' a) h)
                        h
  {-# INLINE (<*) #-}


instance Alternative Parser where
  empty = unexpected "Alternative.empty"
  {-# INLINE empty #-}
  prsr <|> prsr' = Parser $ \ls n i c h →
               runParser prsr ls n i c $ \ls' n' i' e →
                 if n' == n then runParser prsr' ls n' i' c h
                            else h ls' n' i' e
  {-# INLINE (<|>) #-}

instance Parsing Parser where
  try prsr = Parser $ \ ls n i c h →
            runParser prsr ls n i c (\ ls' _ _ e → h ls' n i e)
  {-# INLINE try #-}
  prsr <?> l = Parser $ \ ls n i c h →
              runParser prsr (l : ls) n i (\ _ n' i' a → c ls n' i' a) h
  {-# INLINE (<?>) #-}
  skipMany prsr = Parser $ \ ls n i c h →
                 runParser prsr ls n i
                   (\ ls' n' i' _ → runParser (skipMany prsr) ls' n' i' c h)
                   (\ ls' n' i' _ → c ls' n' i' ())
  skipSome prsr = prsr ⋫ skipMany prsr
  {-# INLINE skipSome #-}
  unexpected e = Parser $ \ ls n i _ h → h ls n i e
  {-# INLINE unexpected #-}
  eof = Parser $ \ ls n i c h → case i of
                   [] → c ls n i ()
                   _  → h ls n i "Parsing.eof"
  {-# INLINABLE eof #-}
  notFollowedBy prsr = Parser $ \ ls n i c h →
                         runParser prsr ls n i
                                   (\ _ _ _ _ → h ls n i "Parsing.notFollowedBy")
                                   (\ _ _ _ _ → c ls n i ())
  {-# INLINE notFollowedBy #-}

instance CharParsing Parser where
  satisfy f = Parser $ \ ls n i c h → case i of
                         x : xs | f x → c ls n' xs x
                                          where !n' = n + 1
                         _ → h ls n i "CharParsing.satisfy"
  {-# INLINABLE satisfy #-}
  string s = Parser $ \ ls n i c h → case stripPrefix s i of
                        𝕵 i'  → c ls n' i' s
                                  where !n' = n + fromIntegral (length s)
                        𝕹    → h ls n i "CharParsing.string"
  {-# INLINABLE string #-}

instance Monad Parser where
  return = pure
  {-# INLINE return #-}
  prsr >>= f = Parser $ \ ls n i c h →
                 runParser prsr ls n i
                           (\ ls' n' i' a → runParser (f a) ls' n' i' c h) h
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}

instance MonadFail Parser where
  fail = unexpected
  {-# INLINE fail #-}

------------------------------------------------------------

{-| run a `Parser` -}
parse ∷ Parser α → 𝕊 → Parsed α
parse prsr i = runParser prsr [] 0 i (\ _  _ _ a → Parsed a)
                                     (\ ls _ _ e → Malformed (reverse ls) e)

--------------------

-- | Parse a `𝕊` to extract the `Data.Textual.Textual` value.
parseString ∷ TextualPlus α ⇒ 𝕊 → Parsed α
parseString = parse $ textual' ⋪ eof
{-# INLINE parseString #-}

--------------------

-- | Parse a '𝕋' to extract the `Data.Textual.Textual` value.
parseText ∷ TextualPlus α ⇒ 𝕋 → Parsed α
parseText = parseString . Text.unpack
{-# INLINE parseText #-}

--------------------

-- | Parse a 'TL.Text' to extract the `Data.Textual.Textual` value.
parseLazyText ∷ TextualPlus α ⇒ TL.Text → Parsed α
parseLazyText = parseString . TL.unpack
{-# INLINE parseLazyText #-}

--------------------

{-| decode and parse a UTF-8 'BS.ByteString' to extract the
    `Data.Textual.Textual` value -}
parseUtf8 ∷ TextualPlus α ⇒ BS.ByteString → Parsed α
parseUtf8 = parseLazyText . decodeUtf8 . BL.fromStrict
{-# INLINE parseUtf8 #-}

----------------------------------------

{- | `parseText`, but in a Monadic context, so that a parse failure becomes a
     `fail`
 -}
parseTextM ∷ (TextualPlus α, MonadFail η) ⇒ 𝕋 → 𝕋 → η α
parseTextM s t =
  case parseText t of
    Parsed    mac → return mac
    Malformed _ e → fail $ [fmt|failed to parse %t: %T (%t)|] s e t

------------------------------------------------------------

{-| types that may be parsed to produce some `TextualPlus` @α@ -}
class ParseableInput α where
  tparse  ∷ ∀ ε β α η .
            (TextualPlus β, AsTextualParseError ε, MonadError ε η) ⇒ α → η β
  tparse' ∷ ∀ β α η .
            (TextualPlus β, MonadError TextualParseError η) ⇒ α → η β
  tparse' = tparse

instance ParseableInput 𝕊 where
  tparse = tparseToME ∘ parseString

instance ParseableInput 𝕋 where
  tparse = tparseToME ∘ parseText

------------------------------------------------------------

newtype P α = P α
  deriving (Eq)

instance Printable α ⇒ Printable (P (Parsed α)) where
  print (P (Parsed a))       = P.string (toString a)
  print (P (Malformed [] s)) = P.text $ "MALFORMED: " <> quote s
  print (P (Malformed ss s)) =
    let bracketsp  = encompass "[ " " ]"
        list    ts = bracketsp $ intercalate ", " (quote ⊳ ts)
     in P.text $ "MALFORMED: " <> quote s <> " " <> list ss

--------------------

newtype ShowEqPrintable α = ShowEqPrintable α
  deriving (Eq)

instance Printable α ⇒ Show (ShowEqPrintable α) where
  show (ShowEqPrintable a) = toString a

{- | Pronounced "test", this tests for equality; it's a variant of
     `(Test.Tasty.HUnit.@=?)` that uses `Printable` rather than `Show` for error
     messages; note that the "got" or "actual" value is the last argument, to
     allow for easier partial application.
 -}
{-
infix 1 ≟
(≟) ∷ (Eq α, Printable α,HasCallStack) ⇒ α → α → Assertion
x ≟ y = ShowEqPrintable x @=? ShowEqPrintable y
-}
----------------------------------------

{- | Almost-synonym for `===`, but using `Printable` instead of `Show`. -}
infix 4 ≣
(≣) ∷ (Eq α, Printable α, HasCallStack) ⇒ α → α → Property
x ≣ y = ShowEqPrintable x === ShowEqPrintable y

{-| property test that parsing & printing printing a random 𝕊 is invertible -}
propInvertibleString ∷ (Eq α, Printable α, TextualPlus α) ⇒ α → Property
propInvertibleString d = P (parseString (toString d)) ≣ P (Parsed d)

--------------------

{-| property test that parsing & printing printing a random 𝕋 is invertible -}
propInvertibleText ∷ (Eq α, Printable α, TextualPlus α) ⇒ α → Property
propInvertibleText d = P (parseText (toText d)) ≣ P (Parsed d)

--------------------

{-| property test that parsing & printing printing a random ByteString as UTF8
    is invertible -}
propInvertibleUtf8 ∷ (Eq α, Printable α, TextualPlus α) ⇒ α → Property
propInvertibleUtf8 d = P (parseUtf8 (toUtf8 d)) ≣ P (Parsed d)

----------------------------------------

{-| test helper that parsing "input" produces an expected value -}
checkT ∷ (TextualPlus α, Eq α, Show α) ⇒ 𝕋 → α → TestTree
checkT input exp =
  testCase ("parseText: " ⊕ unpack input) $
    𝕽 exp @=? (tparseToME' ∘ parseText) input

----------------------------------------

{-| Parse a value with a `Parser`; fail in a `MonadFail` context.
    `tname` is a typename to help the errmsg; use an empty string to silence.
-}
parseT ∷ ∀ α τ η . (Printable τ, MonadFail η) ⇒ Parser α → 𝕋 → τ → η α
parseT prsr tname (toText → t) =
  case TextualPlus.parse prsr (unpack t) of
    Parsed    x → return x
    Malformed es e → case tname of
                       "" → fail $ [fmt|failed to parse '%t': %T (%t)|]
                                   t e (intercalate "‖" $ toText ⊳ es)
                       _  → fail $ [fmt|failed to parse '%t' as %t: %T (%t)|]
                                   t tname e (intercalate "‖" $ toText ⊳ es)

-- that's all, folks! ----------------------------------------------------------
