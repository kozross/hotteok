{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Ascii
  ( -- * Type
    AsciiText,

    -- * Creation
    empty,
    singleton,

    -- * Basic interface
    cons,
    snoc,
    uncons,
    unsnoc,
    length,

    -- * Transformations
    map,
    intercalate,
    intersperse,
    transpose,
    reverse,

    -- * Folds
    foldl,
    foldl',
    foldr,
    foldr',

    -- ** Special folds
    concat,
    concatMap,

    -- * Construction

    -- ** Scans
    scanl,
    scanr,

    -- ** Accumulating maps
    mapAccumL,
    mapAccumR,

    -- ** Generation and unfolding
    unfoldr,

    -- * Substrings

    -- ** Breaking strings
    take,
    drop,
    takeWhile,
    dropWhile,
    splitAt,
    break,
    span,
    group,
    groupBy,
    inits,
    tails,

    -- ** Breaking into many substrings
    split,

    -- ** Breaking into lines and words

    -- * View patterns
    stripPrefix,
    stripSuffix,

    -- * Searching
    filter,
    find,
    partition,

    -- * Indexing
    findIndex,

    -- * Zipping
    zip,

    -- * Conversions
    fromText,
    fromByteString,
    toText,
    toByteString,

    -- * Optics
    textWise,
    byteStringWise,
  )
where

import Control.Category ((.))
import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
import Data.Bifunctor (bimap)
import Data.Binary (Binary (get, put, putList))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAscii)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe (Just, Nothing), fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Exts (IsList (Item, fromList, fromListN, toList))
import Optics.Prism (Prism', prism')
import Text.Ascii.Char.Internal (AsciiChar (AsciiChar))
import Prelude
  ( Bool,
    Eq,
    Int,
    Monoid,
    Ord,
    Read,
    Semigroup,
    Show,
    not,
    pure,
    ($),
    (-),
    (<$>),
    (>),
  )

-- Type

-- | A string of ASCII characters, represented as a packed byte array.
--
-- @since 1.0.0
newtype AsciiText = AsciiText ByteString
  deriving
    ( -- | @since 1.0.0
      Eq,
      -- | @since 1.0.0
      Ord,
      -- | @since 1.0.0
      NFData,
      -- | @since 1.0.0
      Semigroup,
      -- | @since 1.0.0
      Monoid
    )
    via ByteString
  deriving stock
    ( -- | @since 1.0.0
      Show,
      -- | @since 1.0.0
      Read
    )

-- | @since 1.0.0
instance IsList AsciiText where
  type Item AsciiText = AsciiChar -- ^ @since 1.0.0
  {-# INLINEABLE fromList #-}
  fromList = fromJust . fromByteString . fromList . coerce -- ^ @since 1.0.0
  {-# INLINEABLE fromListN #-}
  fromListN n = fromJust . fromByteString . fromListN n . coerce -- ^ @since 1.0.0
  {-# INLINEABLE toList #-}
  toList = coerce . toList . toByteString -- ^ @since 1.0.0

-- | @since 1.0.0
instance Binary AsciiText where
  {-# INLINEABLE put #-}
  -- | @since 1.0.0
  put (AsciiText bs) = do
    let len :: Int = BS.length bs
    put len
    traverse_ (put . BS.index bs) [0 .. len - 1]
  {-# INLINEABLE get #-}
  -- | @since 1.0.0
  get = do
    len :: Int <- get
    fromListN len <$> replicateM len get
  {-# INLINEABLE putList #-}
  -- | @since 1.0.0
  putList ats = putList (coerce @_ @[ByteString] ats)

-- Creation

-- | The empty text.
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
empty :: AsciiText
empty = AsciiText BS.empty

-- | A text consisting of a single ASCII character.
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
singleton :: AsciiChar -> AsciiText
singleton (AsciiChar w8) = AsciiText . BS.singleton $ w8

-- TODO: Quasiquoter

-- Basic interface

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

-- TODO: takeEnd

drop :: Int -> AsciiText -> AsciiText
drop = coerce BS.drop

-- TODO: dropEnd

takeWhile :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
takeWhile f (AsciiText at) = AsciiText . BS.takeWhile (coerce f) $ at

-- TODO: takeWhileEnd

dropWhile :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
dropWhile f (AsciiText at) = AsciiText . BS.dropWhile (coerce f) $ at

-- TODO: dropWhileEnd, dropAround, strip, stripStart, stripEnd

splitAt :: Int -> AsciiText -> (AsciiText, AsciiText)
splitAt = coerce BS.splitAt

-- TODO: breakOn, breakOnEnd

break :: (AsciiChar -> Bool) -> AsciiText -> (AsciiText, AsciiText)
break = coerce BS.break

span :: (AsciiChar -> Bool) -> AsciiText -> (AsciiText, AsciiText)
span = coerce BS.span

group :: AsciiText -> [AsciiText]
group = coerce BS.group

groupBy :: (AsciiChar -> AsciiChar -> Bool) -> AsciiText -> [AsciiText]
groupBy = coerce BS.groupBy

inits :: AsciiText -> [AsciiText]
inits = coerce BS.inits

tails :: AsciiText -> [AsciiText]
tails = coerce BS.tails

-- TODO: splitOn

split :: (AsciiChar -> Bool) -> AsciiText -> [AsciiText]
split = coerce BS.splitWith

-- TODO: chunksOf, lines, words, unlines, unwords

stripPrefix :: AsciiText -> AsciiText -> Maybe AsciiText
stripPrefix = coerce BS.stripPrefix

stripSuffix :: AsciiText -> AsciiText -> Maybe AsciiText
stripSuffix = coerce BS.stripSuffix

-- TODO: stripInfix, commonPrefixes

filter :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
filter = coerce BS.filter

-- TODO: breakOnAll

find :: (AsciiChar -> Bool) -> AsciiText -> Maybe AsciiChar
find = coerce BS.find

partition :: (AsciiChar -> Bool) -> AsciiText -> (AsciiText, AsciiText)
partition = coerce BS.partition

-- TODO: index, safe only

findIndex :: (AsciiChar -> Bool) -> AsciiText -> Maybe Int
findIndex = coerce BS.findIndex

-- TODO: count

zip :: AsciiText -> AsciiText -> [(AsciiChar, AsciiChar)]
zip = coerce BS.zip

-- TODO: zipWith

-- Conversions

fromText :: Text -> Maybe AsciiText
fromText t = case T.find (not . isAscii) t of
  Nothing -> pure . AsciiText . encodeUtf8 $ t
  Just _ -> Nothing

fromByteString :: ByteString -> Maybe AsciiText
fromByteString bs = case BS.find (> 127) bs of
  Nothing -> pure . AsciiText $ bs
  Just _ -> Nothing

toText :: AsciiText -> Text
toText (AsciiText bs) = decodeUtf8 bs

toByteString :: AsciiText -> ByteString
toByteString = coerce

-- Prisms

textWise :: Prism' Text AsciiText
textWise = prism' toText fromText

byteStringWise :: Prism' ByteString AsciiText
byteStringWise = prism' toByteString fromByteString
