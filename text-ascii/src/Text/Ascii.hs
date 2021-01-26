{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Ascii
  ( AsciiText,
    empty,
    singleton,
    cons,
    snoc,
    uncons,
    unsnoc,
    length,
    map,
    intercalate,
    intersperse,
    transpose,
    reverse,
    foldl,
    foldl',
    foldr,
    foldr',
    concat,
    concatMap,
    scanl,
    scanr,
    mapAccumL,
    mapAccumR,
    unfoldr,
    take,
  )
where

import Control.Category ((.))
import Control.DeepSeq (NFData)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Text.Ascii.Char.Internal (AsciiChar (AsciiChar))
import Prelude
  ( Eq,
    Int,
    Maybe,
    Monoid,
    Ord,
    Read,
    Semigroup,
    Show,
    ($),
    (<$>),
  )

newtype AsciiText = AsciiText ByteString
  deriving (Eq, Ord, NFData, Semigroup, Monoid) via ByteString
  deriving stock (Show, Read)

empty :: AsciiText
empty = AsciiText BS.empty

singleton :: AsciiChar -> AsciiText
singleton (AsciiChar w8) = AsciiText . BS.singleton $ w8

cons :: AsciiChar -> AsciiText -> AsciiText
cons (AsciiChar w8) (AsciiText bs) = AsciiText . BS.cons w8 $ bs

snoc :: AsciiText -> AsciiChar -> AsciiText
snoc (AsciiText bs) (AsciiChar w8) = AsciiText . BS.snoc bs $ w8

uncons :: AsciiText -> Maybe (AsciiChar, AsciiText)
uncons (AsciiText bs) = bimap AsciiChar AsciiText <$> BS.uncons bs

unsnoc :: AsciiText -> Maybe (AsciiText, AsciiChar)
unsnoc (AsciiText bs) = bimap AsciiText AsciiChar <$> BS.unsnoc bs

length :: AsciiText -> Int
length (AsciiText bs) = BS.length bs

-- Transformations

map :: (AsciiChar -> AsciiChar) -> AsciiText -> AsciiText
map f (AsciiText bs) = AsciiText . BS.map (coerce f) $ bs

intercalate :: AsciiText -> [AsciiText] -> AsciiText
intercalate (AsciiText bs) = AsciiText . BS.intercalate bs . coerce

intersperse :: AsciiChar -> AsciiText -> AsciiText
intersperse (AsciiChar w8) = AsciiText . BS.intersperse w8 . coerce

transpose :: [AsciiText] -> [AsciiText]
transpose = coerce BS.transpose

reverse :: AsciiText -> AsciiText
reverse = coerce BS.reverse

-- TODO: Replace, justifyLeft, justifyRight, center

foldl :: (a -> AsciiChar -> a) -> a -> AsciiText -> a
foldl f x (AsciiText bs) = BS.foldl (coerce f) x bs

foldl' :: (a -> AsciiChar -> a) -> a -> AsciiText -> a
foldl' f x (AsciiText bs) = BS.foldl' (coerce f) x bs

foldr :: (AsciiChar -> a -> a) -> a -> AsciiText -> a
foldr f x (AsciiText bs) = BS.foldr (coerce f) x bs

foldr' :: (AsciiChar -> a -> a) -> a -> AsciiText -> a
foldr' f x (AsciiText bs) = BS.foldr' (coerce f) x bs

concat :: [AsciiText] -> AsciiText
concat = coerce BS.concat

concatMap :: (AsciiChar -> AsciiText) -> AsciiText -> AsciiText
concatMap = coerce BS.concatMap

scanl :: (AsciiChar -> AsciiChar -> AsciiChar) -> AsciiChar -> AsciiText -> AsciiText
scanl = coerce BS.scanl

scanr :: (AsciiChar -> AsciiChar -> AsciiChar) -> AsciiChar -> AsciiText -> AsciiText
scanr = coerce BS.scanr

mapAccumL :: (a -> AsciiChar -> (a, AsciiChar)) -> a -> AsciiText -> (a, AsciiText)
mapAccumL f x (AsciiText bs) = AsciiText <$> BS.mapAccumL (coerce f) x bs

mapAccumR :: (a -> AsciiChar -> (a, AsciiChar)) -> a -> AsciiText -> (a, AsciiText)
mapAccumR f x (AsciiText bs) = AsciiText <$> BS.mapAccumL (coerce f) x bs

-- TODO: replicate

unfoldr :: (a -> Maybe (AsciiChar, a)) -> a -> AsciiText
unfoldr f = AsciiText . BS.unfoldr (coerce f)

-- TODO: unfoldrN

take :: Int -> AsciiText -> AsciiText
take = coerce BS.take

-- TODO: takeEnd onwards
