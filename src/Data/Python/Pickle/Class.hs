{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications #-}

module Data.Python.Pickle.Class where

import Control.Applicative((<|>), liftA2)
import Control.Monad((>=>), ap)
import Control.Monad.ST.Trans(STT, newSTRef, readSTRef, runSTT)
import Control.Monad.Trans.Class(MonadTrans, lift)
import Control.Monad.Trans.State.Strict(StateT, evalStateT, get, gets, modify, put)

import Data.Binary(Get, Put, getWord8, putWord8)
import Data.Binary.Get(getByteString, getWord16le, getInt32le)
import Data.Bool(bool)
import Data.ByteString.UTF8(toString)
import Data.Either(either)
import Data.Int(Int8, Int16, Int32, Int64)
import Data.List.NonEmpty(NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe(listToMaybe)
import Data.STRef(STRef)
import Data.Typeable(Typeable, typeOf)
import Data.Word(Word8, Word16, Word32, Word64)

traverse2 :: Applicative f => (a -> f b) -> (a, a) -> f (b, b)
traverse2 f (x1, x2) = liftA2 (,) (f x1) (f x2)

data PyObj
  = PyNone
  | PyInteger Integer
  | PyInt Int32
  | PyByte Word8
  | PyUShort Word16
  | PyStr String
  | PyTup [PyObj]
  | PyDict [(PyObj, PyObj)]
  | PyList [PyObj]
  | PyBool Bool
  deriving (Eq, Show)

-- Immutable PyMObjects
data PyMObj s
  = PyObj PyObj -- immutable items
  | PyMTup (Mack s)
  | PyMRef (PyMData s)
  deriving Eq

-- data that should be multable
-- list items are in revserse to
-- implement fast appends, which are
-- prepends in PyMData'
data PyMData' s
  = PyMDict (PyKvps s)
  | PyMList (Mack s)
  deriving Eq

type PyMData s = STRef s (PyMData' s)
type Mack s = [PyMObj s]
type PyKvp s = (PyMObj s, PyMObj s)
type PyKvps s = [PyKvp s]
type Stack s = NonEmpty (Mack s)

freezePy :: Monad m => PyMObj s -> STT s m PyObj
freezePy (PyMTup o) = PyTup <$> (traverse freezePy o)
freezePy (PyMRef r) = readSTRef r >>= freezePy'
freezePy (PyObj o) = pure o

freezePy' :: Monad m => PyMData' s -> STT s m PyObj
freezePy' (PyMList ds) = PyList . reverse <$> (traverse freezePy ds)
freezePy' (PyMDict ds) = PyDict . reverse <$> (traverse (traverse2 freezePy) ds)

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


newtype Pickled a = Pickled a
data PickleState s = PickleState {
    pickleStack :: Stack s
  , pickleMemory :: [Mack s]
  }

initialState :: PickleState s
initialState = PickleState ([] :| []) []

pushOnFrame :: a -> NonEmpty [a] -> NonEmpty [a]
pushOnFrame x (xs :| xss) = (x:xs) :| xss

pushOrInit :: a -> [[a]] -> NonEmpty [a]
pushOrInit x = go
  where go [] = [x] :| []
        go (xs:xss) = (x:xs) :| xss

type PickleT s m a = StateT (PickleState s) m a
type PickleT' s m = PickleT s m ()
type PickleM s m a = PickleT s (STT s m) a
type PickleM' s m = PickleM s m ()
type PickM s a = PickleM s Get a
type PickM' s = PickM s ()
type Failable a = Either String a

orFail :: MonadFail m => (a -> m b) -> Failable a -> m b
orFail f = go
  where go (Left l) = fail l
        go (Right r) = f r

onStack :: (Stack s -> Stack s) -> PickleState s -> PickleState s
onStack f s@PickleState{ pickleStack=ps } = s {pickleStack=f ps }

modifyOnStack :: Monad m => (Stack s -> Stack s) -> PickleT' s m
modifyOnStack = modify . onStack

putStack :: Monad m => PickleState s -> Stack s -> PickleT' s m
putStack s r = put s { pickleStack=r}

-- if this can fail
modifyOnStack' :: MonadFail m => (Stack s -> Failable (Stack s)) -> PickleT' s m
modifyOnStack' f = do
  s@PickleState { pickleStack=ps } <- get
  orFail (putStack s) (f ps)

setStack :: Monad m => PickleState s -> Stack s -> PickleT' s m
setStack s ps = put s {pickleStack=ps}

getStack :: Monad m => PickleT s m (Stack s)
getStack = pickleStack <$> get

getStack' :: Monad m => PickleT s m (PickleState s, Stack s)
getStack' = ap (,) pickleStack <$> get

modifyOnStackM :: Monad m => (Stack s -> PickleT s m (Stack s)) -> PickleT' s m
modifyOnStackM f = do
  (s, ps) <- getStack'
  f ps >>= setStack s

modifyOnStackF :: MonadFail m => (Stack s -> Failable (Stack s)) -> PickleT' s m
modifyOnStackF f = do
  (s, ps) <- getStack'
  orFail (setStack s) (f ps)


modifyOnStackM' :: MonadFail m => (Stack s -> PickleT s m (Failable (Stack s))) -> PickleT' s m
modifyOnStackM' f = do
  s@PickleState{pickleStack=ps} <- get
  f ps >>= orFail (putStack s)


modifyOnTopFrame :: Monad m => (Mack s -> Mack s) -> PickleM s m ()
modifyOnTopFrame f = modifyOnStack (\(x :| xs) -> f x :| xs)

modifyOnTopFrameF :: MonadFail m => (Mack s -> Failable (Mack s)) -> PickleM s m ()
modifyOnTopFrameF f = modifyOnStackF (\(x :| xs) -> (:| xs) <$> f x)

mark :: Monad m => PickleM s m ()
mark = modifyOnStack ([] <|)  -- push a new "stack frame"

pushStack' :: Monad m => PyMObj s -> PickleM s m ()
pushStack' = modifyOnStack . pushOnFrame

pushStack :: Monad m => PyObj -> PickleM s m ()
pushStack = pushStack' . PyObj

newData :: Monad m => PyMData' s -> PickleM s m (PyMObj s)
newData = fmap PyMRef . lift . newSTRef

pushNewStack :: Monad m => PyMData' s -> PickleM s m ()
pushNewStack d = newData d >>= pushStack'

stackUnderflow :: String
stackUnderflow = "unpickling stack underflow"

eitherStackUnderflow :: Failable a
eitherStackUnderflow = Left stackUnderflow

failStackUnderflow :: MonadFail m => m a
failStackUnderflow = fail stackUnderflow

-- Opcode: '0', '1'
pop, popMark :: MonadFail m => PickleM s m ()
pop = modifyOnStack' go
  where go ((_:xs) :| xss) = Right (xs :| xss)
        go ([] :| (xs : xss)) = Right (xs :| xss)
        go _ = eitherStackUnderflow
popMark = modifyOnStack' go
  where go (_ :| (x : xs)) = Right (x :| xs)
        go _ = eitherStackUnderflow

fromTopMost, fromTopMost' :: ([a] -> a) -> NonEmpty [a] -> NonEmpty [a]
fromTopMost = fromTopMost' . (. reverse)
fromTopMost' f (x :| xs) = pushOrInit (f x) xs

fromTopMostM', fromTopMostM :: Monad m => ([a] -> m a) -> NonEmpty [a] -> m (NonEmpty [a])
fromTopMostM = fromTopMostM' . (. reverse)
fromTopMostM' f (x :| xs) = (`pushOrInit` xs) <$> f x
fromTopMostMF', fromTopMostMF :: MonadFail m => ([a] -> m (Failable a)) -> NonEmpty [a] -> m (NonEmpty [a])
fromTopMostMF = fromTopMostMF' . (. reverse)
fromTopMostMF' f (x :| xs) = f x >>= either fail (pure . (`pushOrInit` xs))

newFromTopMostM', newFromTopMostM :: Monad m => (Mack s -> PyMData' s) -> Stack s -> PickleM s m (Stack s)
newFromTopMostM' = fromTopMostM' . (newData .)
newFromTopMostM = fromTopMostM . (newData .)
newFromTopMostMF', newFromTopMostMF :: MonadFail m => (Mack s -> Failable (PyMData' s)) -> Stack s -> PickleM s m (Stack s)
newFromTopMostMF' = fromTopMostMF' . ((sequence . fmap newData) .)
newFromTopMostMF = fromTopMostMF . ((sequence . fmap newData) .)


fromTopMostStack, fromTopMostStack' :: Monad m => (Mack s -> PyMObj s) -> PickleM s m ()
fromTopMostStack = modifyOnStack . fromTopMost
fromTopMostStack' = modifyOnStack . fromTopMost'

fromTopMostStackM, fromTopMostStackM' :: Monad m => (Mack s -> PickleM s m (PyMObj s)) -> PickleM s m ()
fromTopMostStackM = modifyOnStackM . fromTopMostM
fromTopMostStackM' = modifyOnStackM . fromTopMostM'

newFromTopMostStackM, newFromTopMostStackM' :: Monad m => (Mack s -> PyMData' s) -> PickleM s m ()
newFromTopMostStackM = modifyOnStackM . newFromTopMostM
newFromTopMostStackM' = modifyOnStackM . newFromTopMostM'

newFromTopMostStackMF, newFromTopMostStackMF' :: MonadFail m => (Mack s -> Failable (PyMData' s)) -> PickleM s m ()
newFromTopMostStackMF = modifyOnStackM . newFromTopMostMF
newFromTopMostStackMF' = modifyOnStackM . newFromTopMostMF'



-- Opcode: '.'
stop :: MonadFail m => PickleM s m PyObj
stop = do
  d <- gets (listToMaybe . NE.head . pickleStack)
  case d of
    Nothing -> failStackUnderflow
    Just j -> lift (freezePy j)

-- are reversed, since we use dicts in reverse order
toTups :: [a] -> Failable [(a, a)]
toTups [] = Right []
toTups (x1:x2:xs) = ((x2, x1) :) <$> toTups xs  -- value is first on the stack
toTups [_] = Left "odd number of items for DICT."

toDict :: Mack s -> Failable (PyMData' s)
toDict = fmap PyMDict . toTups

_toTup1, _toTup2, _toTup3 :: Mack s -> Failable (Mack s)
_toTup1 (x1:xs) = Right (PyMTup [x1]:xs)
_toTup1 _ = eitherStackUnderflow
_toTup2 (x1:x2:xs) = Right (PyMTup [x1,x2]:xs)
_toTup2 _ = eitherStackUnderflow
_toTup3 (x1:x2:x3:xs) = Right (PyMTup [x1,x2,x3]:xs)
_toTup3 _ = eitherStackUnderflow

-- Opcode: 'N', '}', ')', ']', 'l', 't', '\x85', '\x86', '\x87'
none, emptyDict, emptyTuple, emptyList, list, tuple, dict, tuple1, tuple2, tuple3, true, false :: MonadFail m => PickleM s m ()
none = pushStack PyNone
emptyDict = pushNewStack (PyMDict [])
emptyTuple = pushStack' (PyMTup [])
emptyList = pushNewStack (PyMList [])
list = newFromTopMostStackM' PyMList
tuple = fromTopMostStack PyMTup
dict = newFromTopMostStackMF' toDict
tuple1 = modifyOnTopFrameF _toTup1
tuple2 = modifyOnTopFrameF _toTup2
tuple3 = modifyOnTopFrameF _toTup3
true = pushStack (PyBool True)
false = pushStack (PyBool False)

pushRead :: (a -> PyObj) -> Get a -> PickM' s
pushRead f g = lift (lift g) >>= pushStack . f

-- opcode: 'J', 'K', 'M', '\x8c'
word32, word8, word16, utf8lenstr :: PickM' s
word32 = pushRead PyInt getInt32le
word8 = pushRead PyByte getWord8
word16 = pushRead PyUShort getWord16le
utf8lenstr = pushRead (PyStr . toString) (getWord8 >>= getByteString . fromIntegral)

-- Opcode: '2'
dup :: Monad m => PickleM s m ()
dup = modifyOnStack go
  where go ([] :| xs) = [] :| ([] : xs)  -- duplicate marker
        go ((x:xs) :| xss) = (x : x : xs) :| xss


process' :: Word8 -> PickM' s
process'  40 = mark        -- b'('
process'  41 = emptyTuple  -- b')'
process'  48 = pop         -- b'0'
process'  49 = popMark     -- b'1'
process'  50 = dup         -- b'2'
process'  74 = word32      -- b'J'
process'  75 = word8       -- b'K'
process'  77 = word16      -- b'M'
process'  78 = none        -- b'N'
process'  93 = emptyList   -- b']'
process' 100 = dict        -- b'd'
process' 108 = list        -- b'l'
process' 116 = tuple       -- b't'
process' 125 = emptyDict   -- b'}'
process' 133 = tuple1      -- b'\x85'
process' 134 = tuple2      -- b'\x86'
process' 135 = tuple3      -- b'\x87'
process' 136 = true        -- b'\x88'
process' 137 = false       -- b'\x89'
process' 140 = utf8lenstr  -- b'\x8c'
process' n = fail ("invalid load key, " ++ show n ++ ".")

process :: Word8 -> PickM s PyObj
process 46 = stop
process n = process' n >> parsePickle''  -- handle and continue

parsePickle'' :: PickM s PyObj
parsePickle'' = lift (lift getWord8) >>= process

parsePickle' :: Get PyObj
parsePickle' = runSTT (evalStateT parsePickle'' initialState)

parsePickle :: TryFromPy a => Get a
parsePickle = parsePickle' >>= orFail pure . tryFromPy


assumeG :: Word8 -> Get ()
assumeG w = do
  w' <- getWord8
  if w' == w then pure () else fail "Protocol error"


class PickleTo a b | a -> b where
  toPickleClass :: a -> b


pickleHeader :: Word8 -> Put
pickleHeader = (>>) (putWord8 80) . putWord8

defaultPickleVersion :: Word8
defaultPickleVersion = 4
