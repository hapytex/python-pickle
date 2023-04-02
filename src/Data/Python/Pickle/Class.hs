{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications #-}

module Data.Python.Pickle.Class where

import Control.Applicative((<|>), liftA2)
import Control.Monad((>=>))

import Data.Binary(Binary(get, put), Get, getWord8)
import Data.Bool(bool)
import Data.Int(Int8, Int16, Int32, Int64)
import Data.Typeable(Typeable, typeOf)
import Data.Word(Word8, Word16, Word32, Word64)

import Data.Python.Pickle.Core(
    Failable
  , PyObj(PyBool, PyByte, PyInt, PyInteger, PyList, PyNone, PyTup, PyUShort)
  , orFail, parsePickle'
  )

boundedIntegral :: forall a . (Bounded a, Integral a, Typeable a) => Integer -> Failable a
boundedIntegral v
  | fromIntegral (minBound @a) <= v && v <= fromIntegral (maxBound @a) = Right (fromInteger v)
  | otherwise = Left ("The value " ++ show v ++ " can not be converted to a " ++ show (typeOf @a undefined))

convertBoundedIntegrals :: (Bounded a, Integral a, Typeable a) => PyObj -> Failable a
convertBoundedIntegrals = tryFromPy >=> boundedIntegral

class ToPy a where
  toPy :: a -> PyObj


class ToPy a => TryFromPy a where
  tryFromPy :: PyObj -> Failable a


instance ToPy PyObj where
  toPy = id

instance TryFromPy PyObj where
  tryFromPy = Right


decodeError :: forall a b. (Typeable b, Show a) => a -> Failable b
decodeError v = Left ("Can not decode " ++ show v ++ " to " ++ show (typeOf @b undefined))

instance ToPy Bool where
  toPy = PyBool

instance TryFromPy Bool where
  tryFromPy (PyBool b) = Right b
  tryFromPy v = decodeError v

instance (TryFromPy a, Typeable a) => TryFromPy [a] where
  tryFromPy (PyList ls) = traverse tryFromPy ls
  tryFromPy (PyTup ls) = traverse tryFromPy ls
  tryFromPy v = decodeError v

instance ToPy Integer where
  toPy = PyInteger

instance TryFromPy Integer where
  tryFromPy (PyInteger i) = Right i
  tryFromPy (PyInt i) = Right (fromIntegral i)
  tryFromPy (PyByte i) = Right (fromIntegral i)
  tryFromPy (PyUShort i) = Right (fromIntegral i)
  tryFromPy (PyBool b) = Right (bool 0 1 b)
  tryFromPy v = Left ("Can not convert " ++ show v ++ " to an integral type.")


instance ToPy a => ToPy (Maybe a) where
  toPy Nothing = PyNone
  toPy (Just v) = toPy v

instance (ToPy a, ToPy b) => ToPy (Either a b) where
  toPy (Left l) = toPy l
  toPy (Right r) = toPy r

instance TryFromPy a => TryFromPy (Maybe a) where
  tryFromPy PyNone = Right Nothing
  tryFromPy v = Just <$> tryFromPy v

instance (TryFromPy a, TryFromPy b) => TryFromPy (Either a b) where
  tryFromPy v = Left <$> tryFromPy v <|> Right <$> tryFromPy v

instance ToPy a => ToPy [a] where
  toPy = PyList . map toPy

instance ToPy Int where
  toPy = PyInt . fromIntegral

instance TryFromPy Int where
  tryFromPy = convertBoundedIntegrals

instance ToPy Int8 where
  toPy = PyInt . fromIntegral

instance TryFromPy Int8 where
  tryFromPy = convertBoundedIntegrals

instance ToPy Int16 where
  toPy = PyInt . fromIntegral

instance TryFromPy Int16 where
  tryFromPy = convertBoundedIntegrals

instance ToPy Int32 where
  toPy = PyInt

instance TryFromPy Int32 where
  tryFromPy = convertBoundedIntegrals

instance ToPy Int64 where
  toPy = PyInteger . fromIntegral

instance TryFromPy Int64 where
  tryFromPy = convertBoundedIntegrals

{-
instance TryFromPy Word8 where
  tryFromPy = convertBoundedIntegrals

instance TryFromPy Word16 where
  tryFromPy = convertBoundedIntegrals

instance TryFromPy Word32 where
  tryFromPy = convertBoundedIntegrals

instance TryFromPy Word64 where
  tryFromPy = convertBoundedIntegrals
-}

assumeG :: Word8 -> Get ()
assumeG w = do
  w' <- getWord8
  if w' == w then pure () else fail "Protocol error"

parsePickle :: TryFromPy a => Get a
parsePickle = parsePickle' >>= orFail pure . tryFromPy

newtype Pickled a = Pickled a

instance TryFromPy a => Binary (Pickled a) where
  get = Pickled <$> parsePickle
  put (Pickled p) = undefined
