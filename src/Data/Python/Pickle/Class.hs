{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications #-}

module Data.Python.Pickle.Class where

import Control.Monad.ST.Trans(STT, newSTRef, readSTRef, runSTT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict(StateT, evalStateT, get, gets, modify, put)

import Data.Binary(Get, Put, getWord8, putWord8)
import Data.List.NonEmpty(NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe(listToMaybe)
import Data.STRef(STRef)
import Data.Typeable(Typeable, typeOf)
import Data.Word(Word8)

data PyIObj
  = PyINone
  | PyITup [PyIObj]
  | PyIDict [(PyIObj, PyIObj)]
  | PyIList [PyIObj]
  | PyIBool Bool
  deriving (Eq, Show)

-- Immutable PyObjects
data PyObj s
  = PyNone
  | PyTup [PyObj s]
  | PyRef (PyData s)
  | PyBool Bool
  deriving Eq

-- data that should be multable
-- list items are in revserse to
-- implement fast appends, which are
-- prepends in PyData'
data PyData' s
  = PyDict [(PyObj s, PyObj s)]
  | PyList [PyObj s]
  deriving Eq

type PyData s = STRef s (PyData' s)

freezePy :: Monad m => PyObj s -> STT s m PyIObj
freezePy PyNone = pure PyINone
freezePy (PyTup o) = PyITup <$> (traverse freezePy o)
freezePy (PyRef r) = readSTRef r >>= freezePy'
freezePy (PyBool b) = pure (PyIBool b)

freezePy' :: Monad m => PyData' s -> STT s m PyIObj
freezePy' (PyList d) = PyIList <$> (traverse freezePy d)


class ToPy a where
  toPy :: a -> PyIObj


class TryFromPy a where
  tryFromPy :: PyIObj -> Either String a


instance ToPy PyIObj where
  toPy = id

instance TryFromPy PyIObj where
  tryFromPy = Right


decodeError :: forall a b. (Typeable b, Show a) => a -> Either String b
decodeError v = Left ("Can not decode " ++ show v ++ " to " ++ show (typeOf @b undefined))

instance ToPy Bool where
  toPy = PyIBool

instance TryFromPy Bool where
  tryFromPy (PyIBool b) = Right b
  tryFromPy v = decodeError v

instance (TryFromPy a, Typeable a) => TryFromPy [a] where
  tryFromPy (PyIList ls) = traverse tryFromPy ls
  tryFromPy (PyITup ls) = traverse tryFromPy ls
  tryFromPy v = decodeError v


instance ToPy a => ToPy (Maybe a) where
  toPy Nothing = PyINone
  toPy (Just v) = toPy v

instance (TryFromPy a, Typeable a) => TryFromPy (Maybe a) where
  tryFromPy PyINone = Right Nothing
  tryFromPy v = Just <$> tryFromPy v


instance ToPy a => ToPy [a] where
  toPy = PyIList . map toPy

newtype Pickled a = Pickled a
data PickleState s = PickleState {
    pickleStack :: NonEmpty [PyObj s]
  , pickleMemory :: [PyObj s]
  }

initialState :: PickleState s
initialState = PickleState ([] :| []) []

pushOnFrame :: a -> NonEmpty [a] -> NonEmpty [a]
pushOnFrame x (xs :| xss) = (x:xs) :| xss

pushOrInit :: a -> [[a]] -> NonEmpty [a]
pushOrInit x = go
  where go [] = [x] :| []
        go (xs:xss) = (x:xs) :| xss

type PickleM s m a = StateT (PickleState s) (STT s m) a

onStack :: (NonEmpty [PyObj s] -> NonEmpty [PyObj s]) -> PickleState s -> PickleState s
onStack f s@PickleState{ pickleStack=ps } = s {pickleStack=f ps }

modifyOnStack :: Monad m => (NonEmpty [PyObj s] -> NonEmpty [PyObj s]) -> StateT (PickleState s) m ()
modifyOnStack = modify . onStack

-- if this can fail
modifyOnStack' :: MonadFail m => (NonEmpty [PyObj s] -> Either String (NonEmpty [PyObj s])) -> StateT (PickleState s) m ()
modifyOnStack' f = do
  s@PickleState { pickleStack=ps } <- get
  case f ps of
    Left f -> fail f
    Right r -> put s {pickleStack=r}

modifyOnStackM :: Monad m => (NonEmpty [PyObj s] -> StateT (PickleState s) m (NonEmpty [PyObj s])) -> StateT (PickleState s) m ()
modifyOnStackM f = do
  s@PickleState{pickleStack=ps} <- get
  ps' <- f ps
  put (s {pickleStack=ps'})


modifyOnTopFrame :: Monad m => ([PyObj s] -> [PyObj s]) -> PickleM s m ()
modifyOnTopFrame f = modifyOnStack (\(x :| xs) -> f x :| xs)

mark :: Monad m => PickleM s m ()
mark = modifyOnStack ([] <|)  -- push a new "stack frame"

pushStack :: Monad m => PyObj s -> PickleM s m ()
pushStack = modifyOnStack . pushOnFrame

newData :: Monad m => PyData' s -> PickleM s m (PyObj s)
newData = fmap PyRef . lift . newSTRef

pushNewStack :: Monad m => PyData' s -> PickleM s m ()
pushNewStack d = newData d >>= pushStack

pickleStackUnderflow :: Either String a
pickleStackUnderflow = Left "unpickling stack underflow"

-- Opcode: '0', '1'
pop, popMark :: MonadFail m => PickleM s m ()
pop = modifyOnStack' go
  where go ((_:xs) :| xss) = Right (xs :| xss)
        go ([] :| (xs : xss)) = Right (xs :| xss)
        go _ = pickleStackUnderflow
popMark = modifyOnStack' go
  where go (_ :| (x : xs)) = Right (x :| xs)
        go _ = pickleStackUnderflow

fromTopMost, fromTopMost' :: ([a] -> a) -> NonEmpty [a] -> NonEmpty [a]
fromTopMost = fromTopMost' . (. reverse)
fromTopMost' f (x :| xs) = pushOrInit (f x) xs

fromTopMostM', fromTopMostM :: Monad m => ([a] -> m a) -> NonEmpty [a] -> m (NonEmpty [a])
fromTopMostM = fromTopMostM . (. reverse)
fromTopMostM' f (x :| xs) = (`pushOrInit` xs) <$> f x

newFromTopMostM', newFromTopMostM :: Monad m => ([PyObj s] -> PyData' s) -> NonEmpty [PyObj s] -> PickleM s m (NonEmpty [PyObj s])
newFromTopMostM' = fromTopMostM' . (newData .)
newFromTopMostM = fromTopMostM . (newData .)

fromTopMostStack, fromTopMostStack' :: Monad m => ([PyObj s] -> PyObj s) -> PickleM s m ()
fromTopMostStack = modifyOnStack . fromTopMost
fromTopMostStack' = modifyOnStack . fromTopMost'

fromTopMostStackM, fromTopMostStackM' :: Monad m => ([PyObj s] -> PickleM s m (PyObj s)) -> PickleM s m ()
fromTopMostStackM = modifyOnStackM . fromTopMostM
fromTopMostStackM' = modifyOnStackM . fromTopMostM'

newFromTopMostStackM, newFromTopMostStackM' :: Monad m => ([PyObj s] -> PyData' s) -> PickleM s m ()
newFromTopMostStackM = modifyOnStackM . newFromTopMostM
newFromTopMostStackM' = modifyOnStackM . newFromTopMostM'


-- Opcode: '.'
stop :: Monad m => PickleM s m (Maybe PyIObj)
stop = do
  d <- gets (listToMaybe . NE.head . pickleStack)
  case d of
    Nothing -> pure Nothing
    Just j -> Just <$> lift (freezePy j)

toTups :: [PyObj s] -> Either String [(PyObj s, PyObj s)]
toTups [] = Right []
toTups (x1:x2:xs) = ((x1, x2) :) <$> toTups xs
toTups _ = Left "odd number of items for DICT"

-- Opcode: 'N', '}', ')', ']', 'l', 't', '\x85', '\x86', '\x87'
none, emptyDict, emptyTuple, emptyList, list, tuple, tuple1, tuple2, tuple3, true, false :: Monad m => PickleM s m ()
none = pushStack PyNone
emptyDict = pushNewStack (PyDict [])
emptyTuple = pushStack (PyTup [])
emptyList = pushNewStack (PyList [])
list = newFromTopMostStackM' PyList
tuple = fromTopMostStack PyTup
-- dict = newFromTopMostStackM' (PyDict . toTups)
tuple1 = modifyOnTopFrame (\(x1:xs) -> PyTup [x1] : xs)
tuple2 = modifyOnTopFrame (\(x2:x1:xs) -> PyTup [x1, x2] : xs)
tuple3 = modifyOnTopFrame (\(x3:x2:x1:xs) -> PyTup [x1, x2, x3] : xs)
true = pushStack (PyBool True)
false = pushStack (PyBool False)

-- Opcode: '2'
dup :: Monad m => PickleM s m ()
dup = modifyOnStack go
  where go ([] :| xs) = [] :| ([] : xs)  -- duplicate marker
        go ((x:xs) :| xss) = (x : x : xs) :| xss


process' :: MonadFail m => Word8 -> PickleM s m ()
process'  40 = mark        -- b'('
process'  41 = emptyTuple  -- b')'
process'  48 = pop         -- b'0'
process'  49 = popMark     -- b'1'
process'  50 = dup         -- b'2'
process'  78 = none        -- b'N'
process'  93 = emptyList   -- b']'
process' 108 = list        -- b'l'
process' 116 = tuple       -- b't'
process' 125 = emptyDict   -- b'}'
process' 133 = tuple1      -- b'\x85'
process' 134 = tuple2      -- b'\x86'
process' 135 = tuple3      -- b'\x87'
process' 136 = true        -- b'\x88'
process' 137 = false       -- b'\x89'
process' _ = undefined

process :: Word8 -> PickleM s Get (Maybe PyIObj)
process 46 = stop
process n = process' n >> parsePickle'  -- handle and continue

parsePickle' :: PickleM s Get (Maybe PyIObj)
parsePickle' = lift (lift getWord8) >>= process

parsePickle :: Get (Maybe PyIObj)
parsePickle = runSTT (evalStateT parsePickle' initialState)


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
