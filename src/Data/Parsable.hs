{-# Language ConstrainedClassMethods #-}
{-# Language DefaultSignatures #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language TypeFamilies #-}

-- Needed to remove the warning when we set the default type instance
-- for ParsableInput to ()
{-# Options_GHC -Wno-unused-type-patterns #-}

module Data.Parsable
    ( Parsable(..)
    , PartialParse(..)
    , getPartialParse
    , isCompleteParse
    , satisfyAny
    , someAllowed
    , wordsWithSep
    , endPartial
    , parseCabalPortage
    , parseCabalPortage'
    , module Control.Monad.Except
    , module Text.Parsec
    ) where

import Control.Applicative (some)
import Control.Monad.Except
-- import Control.Monad.Reader
import Data.Functor.Identity
import Data.Kind
import Text.Parsec hiding (runParser)

import Distribution.Cabal.Portage.Error

-- | If a parse succeeds for the beginning of the input, but then fails, we
--   wrap the successful part in the 'PartialParse' constructor. If the entire
--   parse was successful, we wrap it in 'CompleteParse'.
data PartialParse t
    = PartialParse String t
    | CompleteParse t
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- -- | TODO: This assumes combining anything with a 'PartialParse' yields a
-- --   'PartialParse', which may not be reasonable behavior.
-- instance Semigroup t => Semigroup (PartialParse t) where
--     CompleteParse t1 <> CompleteParse t2 = CompleteParse (t1 <> t2)
--     PartialParse  t1 <> CompleteParse t2 = PartialParse (t1 <> t2)
--     CompleteParse t1 <> PartialParse  t2 = PartialParse (t1 <> t2)
--     PartialParse  t1 <> PartialParse  t2 = PartialParse (t1 <> t2)
--
-- -- | TODO: Given the 'Semigroup' instance, the only thing that makes sense
-- --   for 'mempty' is 'CompleteParse'.
-- instance Monoid t => Monoid (PartialParse t) where
--     mempty = CompleteParse mempty

getPartialParse :: PartialParse t -> t
getPartialParse = \case
    PartialParse _ t -> t
    CompleteParse t -> t

isCompleteParse :: PartialParse t -> Bool
isCompleteParse = \case
    PartialParse _ _ -> False
    CompleteParse _ -> True

-- | Represents types that have a valid Parsec parser.
class Parsable t where
    type family ParsableInput t :: Type
    type instance ParsableInput t = ()

    parser :: (Stream s Identity Char, ParsableInput t ~ ())
        => Parsec s () (PartialParse t)
    parser = parser' ()

    parser' :: Stream s Identity Char
        => ParsableInput t -> Parsec s () (PartialParse t)

    default parser' :: (Stream s Identity Char, ParsableInput t ~ ())
        => ParsableInput t -> Parsec s () (PartialParse t)
    parser' _ = parser

    {-# Minimal parser | parser' #-}

-- | Helper function to wrap any parse error in a 'CabalPortageError', then
--   lift to a 'ExceptT' monad transformer.
parseCabalPortage'
    :: (MonadError CabalPortageError m, Stream s Identity Char, Parsable t)
    => ParsableInput t
    -> (ParseError -> CabalPortageError) -- ^ Constructor taking a ParseError
    -> SourceName                        -- ^ A name for where the input comes from
    -> s                                 -- ^ Input to parse
    -> m (PartialParse t)
parseCabalPortage' t c sn = liftEither . either (Left . c) Right . parse (parser' t) sn

parseCabalPortage ::
    ( MonadError CabalPortageError m
    , Stream s Identity Char
    , Parsable t
    , ParsableInput t ~ ()
    ) => (ParseError -> CabalPortageError) -- ^ Constructor taking a ParseError
    -> SourceName                        -- ^ A name for where the input comes from
    -> s                                 -- ^ Input to parse
    -> m (PartialParse t)
parseCabalPortage = parseCabalPortage' ()

-- | Parse a Char that satisfies any of the given predicates
satisfyAny :: Stream s Identity Char => [Char -> Bool] -> Parsec s u Char
satisfyAny fs = satisfy $ \c -> or $ ($c) <$> fs

-- | One or more characters that satisfy any of the given predicates
someAllowed :: Stream s Identity Char
    => [Char -> Bool]
    -> Parsec s u (PartialParse String)
someAllowed allowed = wordsWithSep Nothing allowed allowed []

-- | Best effort parsing of "words" starting with certain allowed characters,
--   then multiple of characters allowed after the start of a "word".
--
--   "Words" are separated by certain allowed characters, such as @'-'@ and
--   @'_'@.
wordsWithSep :: Stream s Identity Char
    -- | Optional beginning of parser, skips normal beginning
    => Maybe (Parsec s u String)
    -> [Char -> Bool]    -- ^ Character allowed at the start of a "word"
    -> [Char -> Bool]    -- ^ Characters allowed after the start of a "word"
    -> [Char -> Bool]    -- ^ Characters that separate "words"
    -> Parsec s u (PartialParse String)
wordsWithSep maybeBeg wordStart wordRest wordSep = do
    beg <- case maybeBeg of
        Just p -> p
        Nothing -> do
            w <- some $ satisfyAny wordStart
            r <- many $ satisfyAny wordRest
            pure $ w ++ r
    choice
        [ try $ do
            sep  <- satisfyAny wordSep
            next <- wordsWithSep Nothing wordStart wordRest wordSep
            pure $ ((beg ++ [sep]) ++) <$> next
        , ($ beg) <$> endPartial
        ]

-- | Return 'CompleteParse' if we are at 'eof', otherwise 'PartialParse'
endPartial :: Stream s Identity Char => Parsec s u (t -> PartialParse t)
endPartial = choice
    [ CompleteParse <$ eof
    , PartialParse <$> lookAhead (some anyChar)
    ]
