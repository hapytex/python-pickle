{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, TypeApplications #-}

module Data.Python.Pickle.Class where

import Control.Monad.Trans.State.Strict(StateT, gets, modify)

import Data.Binary(Get, Put, getWord8, putWord8)
import Data.List.NonEmpty(NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe(listToMaybe)
import Data.Typeable(Typeable)
import Data.Word(Word8)


data PyObj
  = PyNone
  | PyDict [(PyObj, PyObj)]
  | PyTuple [PyObj]
  | PyList [PyObj]
  | PyBool Bool
  deriving Eq


class ToPy a where
  toPy :: a -> PyObj


class TryFromPy a where
  tryFromPy :: PyObj a -> Either String b


instance ToPy Bool where
  toPy = PyBool


decodeError :: (Typeable b, Show a) -> Either String b
decodeError v = Left "Can not decode " ++ show v ++ " to " ++ show (typeOf @b undefined)

instance TryFromPy Bool where
  tryFromPy (PyBool b) = Right b
  tryFromPy _ = Left "Can not decode to Bool"


instance ToPy a => ToPy [a] where
  toPy = PyList . map toPy


newtype Pickled a = Pickled a
data PickleState = PickleState {
    pickleStack :: NonEmpty [PyObj]
  , pickleMemory :: [PyObj]
  }

initialState :: PickleState
initialState = PickleState ([] :| []) []

pushOnFrame :: a -> NonEmpty [a] -> NonEmpty [a]
pushOnFrame x (xs :| xss) = (x:xs) :| xss

pushOrInit :: a -> [[a]] -> NonEmpty [a]
pushOrInit x = go
  where go [] = [x] :| []
        go (xs:xss) = (x:xs) :| xss

type PickleRead m a = StateT PickleState m a

onStack :: (NonEmpty [PyObj] -> NonEmpty [PyObj]) -> PickleState -> PickleState
onStack f s@PickleState{ pickleStack=ps } = s {pickleStack=f ps }

modifyOnStack :: Monad m => (NonEmpty [PyObj] -> NonEmpty [PyObj]) -> PickleRead m ()
modifyOnStack = modify . onStack

-- if this can fail
modifyOnStack' :: MonadFail m => (NonEmpty [PyObj] -> Either (String (NonEmpty [PyObj])) -> PickleRead m ()
modifyOnStack'

modifyOnTopFrame :: Monad m => ([PyObj] -> [PyObj]) -> PickleRead m ()
modifyOnTopFrame f = modifyOnStack (\(x :| xs) -> f x :| xs)

mark :: Monad m => PickleRead m ()
mark = modifyOnStack ([] <|)  -- push a new "stack frame"

pushStack :: Monad m => PyObj -> PickleRead m ()
pushStack = modifyOnStack . pushOnFrame

pickleStackUnderflow :: MonadFail m => m a
pickleStackUnderflow = fail "unpickling stack underflow"

-- Opcode: '0'
pop :: Monad m => PickleRead m ()
pop = modifyOnStack go
  where go ((_:xs) :| xss) = xs :| xss
        go ([] :| (xs : xss)) = xs :| xss
        go _ = pickleStackUnderflow

fromTopMost, fromTopMost' :: ([a] -> a) -> NonEmpty [a] -> NonEmpty [a]
fromTopMost = fromTopMost' . (. reverse)
fromTopMost' f (x :| xs) = pushOrInit (f x) xs

fromTopMostStack :: Monad m => ([PyObj] -> PyObj) -> PickleRead m ()
fromTopMostStack = modifyOnStack . fromTopMost

-- Opcode: '.'
stop :: Monad m => PickleRead m (Maybe PyObj)
stop = gets (listToMaybe . NE.head . pickleStack)

-- Opcode: 'N', '}', ')', ']', 'l', 't', '\x85', '\x86', '\x87'
none, emptyDict, emptyTuple, emptyList, list, tuple, tuple1, tuple2, tuple3, true, false :: Monad m => PickleRead m ()
none = pushStack PyNone
emptyDict = pushStack (PyDict [])
emptyTuple = pushStack (PyTuple [])
emptyList = pushStack (PyList [])
list = fromTopMostStack PyList
tuple = fromTopMostStack PyTuple
tuple1 = modifyOnTopFrame (\(x1:xs) -> PyTuple [x1] : xs)
tuple2 = modifyOnTopFrame (\(x2:x1:xs) -> PyTuple [x1, x2] : xs)
tuple3 = modifyOnTopFrame (\(x3:x2:x1:xs) -> PyTuple [x1, x2, x3] : xs)
true = pushStack (PyBool True)
false = pushStack (PyBool False)

-- Opcode: '2'
dup, append :: Monad m => PickleRead m ()
dup = modifyOnStack go
  where go ([] :| xs) = [] :| ([] : xs)  -- duplicate marker
        go ((x:xs) :| xss) = (x : x : xs) :| xss
append = modifyOnStack go
  where go (x :| (xs:xss)) = (x ++ xs) :| xss
        go _ = pickleStackUnderflow


process :: Monad m => Word8 -> PickleRead m ()
process  40 = mark        -- b'('
process  41 = emptyTuple  -- b')'
process  48 = pop         -- b'0'
process  50 = dup         -- b'2'
process  78 = none        -- b'N'
process  93 = emptyList   -- b']'
process 108 = list        -- b'l'
process 116 = tuple       -- b't'
process 125 = emptyDict   -- b'}'
process 133 = tuple1      -- b'\x85'
process 134 = tuple2      -- b'\x86'
process 135 = tuple3      -- b'\x87'
process 136 = true        -- b'\x88'
process 136 = false       -- b'\x89'
process _ = undefined


assumeG :: Word8 -> Get ()
assumeG w = do
  w' <- getWord8
  if w' == w then pure () else fail "Protocol error"

class ReadWritePickle a where
  writePickle :: a -> Put
  readPickle :: Get a


-- The empty tuple, decoded as ')'
instance ReadWritePickle () where
  writePickle _ = putWord8 41
  readPickle = assumeG 41

instance ReadWritePickle a => ReadWritePickle (Pickled a) where
  writePickle (Pickled a) = pickleHeader defaultPickleVersion >> writePickle a >> putWord8 46  -- 46: . (stop)


class PickleTo a b | a -> b where
  toPickleClass :: a -> b


pickleHeader :: Word8 -> Put
pickleHeader = (>>) (putWord8 80) . putWord8

defaultPickleVersion :: Word8
defaultPickleVersion = 4
