{-|
Module      : Data.Parsable

This module contains two simple classes, 'Parsable' and 'Printable'.

There is an implicit "soft isomorphism" between 'parser' and 'toString'.
(Successfully parsing a string and then running 'toString' on the result
should result in the original string.)

'Parsable' instances must return a 'PartialParse' wrapping the data. This can
be done easily with the 'endPartial' function.

For instance:

> parser = endPartial $ count 5 anyChar

=== Language extensions

Because 'parser' and 'parser'' do not take any arguments, it may be necessary
to explicitly declare the type of @t@ for these functions.

It may be helpful to enable and use the @TypeApplications@ and possibly
@ScopedTypeVariables@ extensions. This extends to helper functions that use
'parser' and 'parser'', such as 'runParsable' and 'runParsable''.

Look at the @Language Extensions@ section of the GHC documentation for
instructions on how to use these extensions.
-}

{-# Language ConstrainedClassMethods #-}
{-# Language DefaultSignatures #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveTraversable #-}
{-# Language DerivingVia #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}

-- Needed to remove the warning when we set the default type instance
-- for ParsableInput to ()
{-# Options_GHC -Wno-unused-type-patterns #-}

module Data.Parsable
    (
    -- * Parsing
      Parsable(..)
    , parserValue
    , parserValue'
    , runParsable
    , runParsable'
    , NaturalParsable(..)
    -- ** Partial parses
    , PartialParse(..)
    , getPartialParse
    , isCompleteParse
    , partialParses
    -- ** Parsing functions
    , satisfyAny
    , someAllowed
    , wordsWithSep
    , wordsWithSep'
    , endPartial
    -- * Printing
    , Printable(..)
    , ShowPrintable(..)
    , toText
    -- * Re-exports
    , module Data.Char
    , module Data.Functor.Identity
    , module Data.Kind
    , module Data.String
    , module Text.Parsec
    , module Control.Applicative
    , module Data.Functor.Slim.Apply
    ) where

import Control.Applicative (some)
import Data.Char
import Data.Functor.Slim.Apply
import Data.Functor.Identity
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup
import Data.String
import Data.Text (Text, unpack)
import GHC.Generics
import Text.Parsec
-- import Text.Parsec.Token (GenTokenParser(natural))
-- import Text.Parsec.Language (haskell)



-- | If a parse succeeds for the beginning of the input, but then fails, we
--   wrap the successful part in the v'PartialParse' constructor. If the entire
--   parse was successful, we wrap it in 'CompleteParse'.
--
--   This is mostly useful for testing parsers where we need to know if the
--   entire test string was parsed or not, but still allows for combining
--   individual parsers into larger ones.
data PartialParse t
    = PartialParse
        String -- ^ The unparsed remainder. Useful for error messages
        t
    | CompleteParse t
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1)
    deriving Semigroup via (Ap' PartialParse t)

--   The general idea is that if many parses end with a v'CompleteParse', the
--   entire sequence of parses is itself a v'CompleteParse'. The opposite holds
--   for v'PartialParse' as well.
instance Apply PartialParse where
    CompleteParse  f <.> CompleteParse  x = CompleteParse  (f x)
    PartialParse _ f <.> CompleteParse  x = CompleteParse  (f x)
    CompleteParse  f <.> PartialParse s x = PartialParse s (f x)
    PartialParse _ f <.> PartialParse s x = PartialParse s (f x)

-- | Extract the data from a 'PartialParse'
getPartialParse :: PartialParse t -> t
getPartialParse = \case
    PartialParse _ t -> t
    CompleteParse t -> t

-- | Returns @True@ on 'CompleteParse' and @False@ on v'PartialParse'
isCompleteParse :: PartialParse t -> Bool
isCompleteParse = \case
    PartialParse _ _ -> False
    CompleteParse _ -> True

-- | Condense a non-empty list of 'PartialParse's to a single 'PartialParse'
--   carrying a non-empty list. Similar to 'sequence'.
partialParses :: NonEmpty (PartialParse a) -> PartialParse (NonEmpty a)
partialParses = sconcat . fmap (fmap NE.singleton)

-- | Represents types that have a valid Parsec parser.
class Parsable t where
    -- | Represents some other input that is needed to create the parser. This
    --   defaults to @()@ and can safely be ignored in most cases, but is
    --   available if needed.
    type family ParsableInput t :: Type
    type instance ParsableInput t = ()

    parser :: (Stream s Identity Char, ParsableInput t ~ ())
        => Parsec s () (PartialParse t)
    parser = parser' ()

    -- | A more general case of 'parser' for when @ParsableInput t@ is /not/ equal
    --   to @()@. This must be defined instead of 'parser' if this is the case.
    parser' :: Stream s Identity Char
        => ParsableInput t -- ^ Input needed to build the 'Parsable' parser
        -> Parsec s () (PartialParse t)

    default parser' :: (Stream s Identity Char, ParsableInput t ~ ())
        => ParsableInput t -- ^ Input needed to build the 'Parsable' parser
        -> Parsec s () (PartialParse t)
    parser' _ = parser

    {-# Minimal parser | parser' #-}

-- | Extract the 'PartialParse' value when running 'parser'. This can be
--   convenient when it doesn't matter if it was a v'PartialParse' or a
--   v'CompleteParse'.
parserValue :: (Parsable t, ParsableInput t ~ (), Stream s Identity Char)
    => Parsec s () t
parserValue = getPartialParse <$> parser

-- | The same as 'parserValue', but takes 'ParsableInput'.
parserValue' :: (Parsable t, Stream s Identity Char)
    => ParsableInput t -> Parsec s () t
parserValue' = fmap getPartialParse . parser'

-- | Convenience method to run a 'Parsable' parser.
runParsable :: forall t s. (Parsable t, Stream s Identity Char, ParsableInput t ~ ())
    => SourceName -- ^ Only used in error messages and may be the empty string
    -> s
    -> Either ParseError (PartialParse t)
runParsable = parse (parser @t)

-- | Same as 'runParsable' but takes a non-trivial 'ParsableInput' as an argument.
runParsable' :: forall t s. (Parsable t, Stream s Identity Char)
    => ParsableInput t -- ^ Input needed to build the 'Parsable' parser
    -> SourceName      -- ^ Only used in error messages and may be the empty string
    -> s
    -> Either ParseError (PartialParse t)
runParsable' i = parse (parser' @t i)

newtype NaturalParsable a = NaturalParsable
    { unwrapNaturalParsable :: a }
    deriving (Read, Show, Eq, Ord, Num)

instance Read a => Parsable (NaturalParsable a) where
    parser = (<?> "natural number") $ endPartial $ do
        x <- some digit
        pure $ read x

-- | Parse a Char that satisfies any of the given predicates
satisfyAny :: Stream s Identity Char => [Char -> Bool] -> Parsec s u Char
satisfyAny fs = satisfy $ \c -> or [f c | f <- fs]

-- | One or more characters that satisfy any of the given predicates
someAllowed :: Stream s Identity Char
    => [Char -> Bool]
    -> Parsec s u (PartialParse String)
someAllowed allowed = wordsWithSep allowed allowed []

-- | Best effort parsing of "words" starting with certain allowed characters,
--   then multiple of characters allowed after the start of a word.
--
--   Words are separated by certain allowed characters, such as @\'-\'@ and
--   @\'_\'@.
--
--   Example:
--
--   > wordsWithSep [isAsciiUpper] [isAsciiUpper, isAsciiLower] [(== '-')]
--
--   This would parse a string such as @"SomeAwesome-Thing"@:
--
--       * The first character of each word must be an upper-case ASCII letter.
--
--       * The remaining characters of each word may be upper- or lower-case
--         letters.
--
--       * Words are separated by @'-'@ characters.
--
--  If a @wordsWithSep@ parser fails, it will return v'PartialParse' wrapping
--  the string up to and including the last successfully parsed word.
wordsWithSep :: Stream s Identity Char
    => [Char -> Bool]    -- ^ Characters allowed at the start of a word
    -> [Char -> Bool]    -- ^ Characters allowed after the start of a word
    -> [Char -> Bool]    -- ^ Characters that separate words
    -> Parsec s u (PartialParse String)
wordsWithSep = wordsWithSepG Nothing

-- | The same as 'wordsWithSep', but this takes a parser which will be run in
--   place of normal word detection for the first word. This allows for
--   starting with generally unallowed characters before the first separator.
wordsWithSep' :: Stream s Identity Char
    => Parsec s u String -- ^ Beginning parser, skips normal beginning
    -> [Char -> Bool]    -- ^ Characters allowed at the start of a word
    -> [Char -> Bool]    -- ^ Characters allowed after the start of a word
    -> [Char -> Bool]    -- ^ Characters that separate words
    -> Parsec s u (PartialParse String)
wordsWithSep' = wordsWithSepG . Just

-- | General version of 'wordsWithSep'. Not exported.
wordsWithSepG :: Stream s Identity Char
    -- | Optional beginning of parser, skips normal beginning
    => Maybe (Parsec s u String)
    -> [Char -> Bool]    -- ^ Characters allowed at the start of a word
    -> [Char -> Bool]    -- ^ Characters allowed after the start of a word
    -> [Char -> Bool]    -- ^ Characters that separate words
    -> Parsec s u (PartialParse String)
wordsWithSepG maybeBeg wordStart wordRest wordSep = do
    beg <- case maybeBeg of
        Just p -> p
        Nothing -> do
            w <- some $ satisfyAny wordStart
            r <- many $ satisfyAny wordRest
            pure $ w ++ r
    choice
        [ try $ do
            sep  <- satisfyAny wordSep
            next <- wordsWithSepG Nothing wordStart wordRest wordSep
            pure $ ((beg ++ [sep]) ++) <$> next
        , endPartial $ pure beg
        ]

-- | Run the specified parser, then return 'CompleteParse' if we are at
--   'eof', otherwise 'PartialParse'.
endPartial :: Stream s Identity Char
    => Parsec s u t
    -> Parsec s u (PartialParse t)
endPartial p = do
    t <- p
    f <- choice
            [ CompleteParse <$ eof
            , PartialParse <$> lookAhead (some anyChar)
            ]
    pure $ f t

-- | Types that can be converted back to a @String@.
class Printable t where
    toString :: t -> String
    {-# Minimal toString #-}

instance Printable String where
    toString = id

instance Printable Text where
    toString = unpack

-- | Convenience method that will turn a 'Printable' to any 'IsString'.
toText :: (Printable t, IsString s) => t -> s
toText = fromString . toString

-- | Wrapper for types that inherit 'toString' directly from their 'Show' instance.
--
--   It is convenient to use the @DerivingVia@ language extension with this.
newtype ShowPrintable a = ShowPrintable {
    unwrapShowPrintable :: a
} deriving (Read, Show, Eq, Ord, Num, IsString)

instance Show a => Printable (ShowPrintable a) where
    toString = show . unwrapShowPrintable
