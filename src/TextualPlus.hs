module TextualPlus
  ( PrintOut( toP ), parseTextual, parseTextM
  , TextualPlus(..), parse, parseString
  , __ERR__, __error__
  , bracket, bracket', b, b'
  , encompass, encompass'
  , guillemet, guillemet', g, g'
  , parenthesize, parenthesize', p, p'
  , quote, quote', q, q'
  , qquote, qquote', qq, qq'
  , surround, surround'

  , propInvertibleString, propInvertibleText, propInvertibleUtf8
  )
where


import Base0
import Prelude  ( error )

-- base --------------------------------

import Control.Applicative  ( Alternative( (<|>), empty ) )
import Control.Monad        ( MonadFail( fail ) )
import Data.Functor         ( Functor )
import Data.List            ( length, reverse, stripPrefix )
import Data.Typeable        ( typeOf )
import Data.Word            ( Word )

-- bytestring --------------------------

import qualified  Data.ByteString       as  BS
import qualified  Data.ByteString.Lazy  as  BL

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ), toUtf8 )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋪), (⋫) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Maybe        ( pattern 𝕵, pattern 𝕹 )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, satisfy, string )
import Text.Parser.Combinators  ( Parsing( notFollowedBy, skipMany, skipSome )
                                , (<?>), eof, try, unexpected )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Property, (===) )

-- text --------------------------------

import qualified  Data.Text       as  Text
import qualified  Data.Text.Lazy  as  TL

import Data.Text                ( Text, intercalate )
import Data.Text.Lazy.Encoding  ( decodeUtf8 )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

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

----------------------------------------

{- | `parseText`, but in a Monadic context, so that a parse failure becomes a
     `fail`
 -}
parseTextM ∷ (TextualPlus α, MonadFail η) ⇒ Text → Text → η α
parseTextM s t =
  case parseText t of
    Parsed    mac → return mac
    Malformed _ e → fail $ [fmt|failed to parse %t: %T (%t)|] s e t

data Parser α =
  Parser { runParser ∷ ∀ r .
                       [String] → Word → String
                     → ([String] → Word → String → α → Parsed r)
                     → ([String] → Word → String → String → Parsed r)
                     → Parsed r }

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

parse ∷ Parser α → String → Parsed α
parse prsr i = runParser prsr [] 0 i (\ _  _ _ a → Parsed a)
                                     (\ ls _ _ e → Malformed (reverse ls) e)

-- | Parse a 'String' to extract the 'Textual' value.
parseString ∷ TextualPlus α ⇒ String → Parsed α
parseString = parse $ textual' ⋪ eof
{-# INLINE parseString #-}

-- | Parse a 'Text.Text' to extract the 'Textual' value.
parseText ∷ TextualPlus α ⇒ Text.Text → Parsed α
parseText = parseString . Text.unpack
{-# INLINE parseText #-}

parseLazyText ∷ TextualPlus α ⇒ TL.Text → Parsed α
parseLazyText = parseString . TL.unpack
{-# INLINE parseLazyText #-}

-- | Decode and parse a UTF-8 'BS.ByteString' to extract the 'Textual' value.
parseUtf8 ∷ TextualPlus α ⇒ BS.ByteString → Parsed α
parseUtf8 = parseLazyText . decodeUtf8 . BL.fromStrict
{-# INLINE parseUtf8 #-}

------------------------------------------------------------

class TextualPlus α where
  textual' ∷ (MonadFail μ, CharParsing μ) ⇒ μ α

------------------------------------------------------------

newtype P α = P α
  deriving Eq

instance Printable α ⇒ Printable (P (Parsed α)) where
  print (P (Parsed a))       = P.string (toString a)
  print (P (Malformed [] s)) = P.text $ "MALFORMED: " <> quote s
  print (P (Malformed ss s)) =
    let bracketsp  = encompass "[ " " ]"
        list    ts = bracketsp $ intercalate ", " (quote ⊳ ts)
     in P.text $ "MALFORMED: " <> quote s <> " " <> list ss

--------------------

newtype ShowEqPrintable α = ShowEqPrintable α
  deriving Eq

instance Printable α ⇒ Show (ShowEqPrintable α) where
  show (ShowEqPrintable a) = toString a

{- | Pronounced 'test', this tests for equality; it's a variant of `(@=?)` that
     uses `Printable` rather than `Show` for error messages; note that the 'got'
     or 'actual' value is the last argument, to allow for easier partial
     application.
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

propInvertibleString ∷ (Eq α, Printable α, TextualPlus α) ⇒ α → Property
propInvertibleString d = P (parseString (toString d)) ≣ P (Parsed d)

--------------------

propInvertibleText ∷ (Eq α, Printable α, TextualPlus α) ⇒ α → Property
propInvertibleText d = P (parseText (toText d)) ≣ P (Parsed d)

--------------------

propInvertibleUtf8 ∷ (Eq α, Printable α, TextualPlus α) ⇒ α → Property
propInvertibleUtf8 d = P (parseUtf8 (toUtf8 d)) ≣ P (Parsed d)

-- that's all, folks! ----------------------------------------------------------
