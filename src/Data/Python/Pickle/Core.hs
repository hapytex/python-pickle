module Data.Python.Pickle.Core where

import Control.Applicative(liftA2)
import Control.Monad(ap)
import Control.Monad.ST.Trans(STT, newSTRef, readSTRef, writeSTRef, runSTT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict(StateT, evalStateT, get, gets, modify, put)

import Debug.Trace(traceShow)

import Data.Binary(Get, Put, getWord8, putWord8)
import Data.Binary.Get(getByteString, getWord16le, getWord32le, getInt32le, getWord64le, getDoublebe)
import Data.ByteString.UTF8(toString)
import Data.HashMap.Strict((!))
import Data.Int(Int32)
import Data.List.NonEmpty(NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe(listToMaybe)
import Data.STRef(STRef)
import Data.Word(Word8, Word16)

import Data.Python.Pickle.Failable(Failable, FailableWith, orFail, orFailWith)
import Data.Python.Pickle.Object(PyMObj(PyObj, PyMRef, PyMTup), PyObj(PyNone, PyBool, PyInt, PyByte, PyUShort, PyStr, PyInteger, PyDouble), Stack, Mack, Memo, PyMData'(PyMDict, PyMList), toPyDict, appendReverse, freezePy)
import Data.Python.Pickle.State(PickleState(PickleState, pickleStack, pickleMemory), initialState, onStack, onMemo, memoizeItem)
import Data.Python.Literals(intParser)

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

modifyOnStack :: Monad m => (Stack s -> Stack s) -> PickleT' s m
modifyOnStack = modify . onStack

modifyOnMemo :: Monad m => (Memo s -> Memo s) -> PickleT' s m
modifyOnMemo = modify . onMemo


putStack :: Monad m => PickleState s -> Stack s -> PickleT' s m
putStack s r = put s { pickleStack=r}

-- if this can fail
modifyOnStack' :: MonadFail m => (Stack s -> Failable (Stack s)) -> PickleT' s m
modifyOnStack' f = do
  s@PickleState { pickleStack=ps } <- get
  orFail (putStack s) (f ps)

modifyOnStackWith' :: MonadFail m => (Stack s -> Failable (a, Stack s)) -> PickleT s m a
modifyOnStackWith' f = do
  s@PickleState { pickleStack=ps } <- get
  orFailWith (putStack s) (f ps)


setStack :: Monad m => PickleState s -> Stack s -> PickleT' s m
setStack s ps = put s {pickleStack=ps}

getStack :: Monad m => PickleT s m (Stack s)
getStack = pickleStack <$> get

getMemo :: Monad m => PickleT s m (Memo s)
getMemo = pickleMemory <$> get

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

notA :: String -> String
notA x = "The given item is not a " ++ x ++ "."

eitherStackUnderflow :: Failable a
eitherStackUnderflow = Left stackUnderflow

failStackUnderflow :: MonadFail m => m a
failStackUnderflow = fail stackUnderflow

_peek :: NonEmpty [a] -> Either String a
_peek ((x:_) :| _) = Right x
_peek _ = Left stackUnderflow

peek :: MonadFail m => PickleM s m (PyMObj s)
peek = getStack >>= either fail pure . _peek

popMark' :: MonadFail m => PickleM s m [PyMObj s]
popMark' = modifyOnStackWith' go
  where go (y :| (x : xs)) = Right (y, (x :| xs))
        go _ = eitherStackUnderflow


-- Opcode: '0', '1'
pop, popMark :: MonadFail m => PickleM s m ()
pop = modifyOnStack' go
  where go ((_:xs) :| xss) = Right (xs :| xss)
        go ([] :| (xs : xss)) = Right (xs :| xss)
        go _ = eitherStackUnderflow
popMark = () <$ popMark'

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
stop = gets (listToMaybe . NE.head . pickleStack) >>= maybe failStackUnderflow (lift . freezePy)

-- are reversed, since we use dicts in reverse order
toTups :: [a] -> Failable [(a, a)]
toTups [] = Right []
toTups (x1:x2:xs) = ((x2, x1) :) <$> toTups xs  -- value is first on the stack
toTups [_] = Left "odd number of items for DICT."

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
dict = newFromTopMostStackMF' toPyDict
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

_append ::  Monad m => STRef s (PyMData' s) -> Mack s -> STT s m ()
_append ls ys = readSTRef ls >>= writeSTRef ls . appendReverse ys

appends :: MonadFail m => PickleM s m ()
appends = do
  xs <- popMark'  -- is reversed
  (PyMRef y) <- peek
  -- TODO: non-dicts?
  lift (_append y xs)

memoize :: MonadFail m => PickleM s m ()
memoize = peek >>= modifyOnMemo . memoizeItem

binput :: (Integral i, MonadFail m) => i -> PickleM s m ()
binput i = peek >>= modifyOnMemo . putItem i

binget :: (Integral i, MonadFail m) => i -> PickleM s m ()
binget i = getMemo >>= pushStack' . (! fromIntegral i)

-- Opcode: '2'
dup :: Monad m => PickleM s m ()
dup = modifyOnStack go
  where go ([] :| xs) = [] :| ([] : xs)  -- duplicate marker
        go ((x:xs) :| xss) = (x : x : xs) :| xss


setItem :: Monad m => PickleM s m ()
setItem = undefined   -- TODO: non-dict?

setItems :: Monad m => PickleM s m ()
setItems = undefined  -- TODO: non-dict?


process' :: Word8 -> PickM' s
process'  40 = mark                            -- b'('
                                               -- b'.'  (stop, not used here)
process'  48 = pop                             -- b'0'
process'  49 = popMark                         -- b'1'
process'  50 = dup                             -- b'2'
-- process'  70 = float                           -- b'F'
process'  73 = pushRead PyInteger intParser    -- b'I'  TODO: 01/00 = True/False
process'  74 = word32                          -- b'J'
process'  75 = word8                           -- b'K'
process'  76 = pushRead PyInteger intParser    -- b'L'  TODO: omit L as optional (!) prefix
process'  77 = word16                          -- b'M'
process'  78 = none                            -- b'N'

process'  93 = emptyList                       -- b']'
process' 100 = dict                            -- b'd'
process' 101 = appends                         -- b'e'
process' 103 = lift (lift getWord8) >>= binget -- b'h'
process' 105 = lift (lift getWord32le) >>= binget  -- b'j'
process' 108 = list                            -- b'l'
process' 113 = lift (lift getWord8) >>= binput -- 'q'
process' 113 = lift (lift getWord32le) >>= binput -- 'q'
process' 115 = setItem                         -- b's'
process' 116 = tuple                           -- b't'
process'  41 = emptyTuple                      -- b')'
process' 117 = setItems                        -- b'u'
process'  71 = pushRead PyDouble getDoublebe   -- b'G'  parse 64-bit floating point number
process' 125 = emptyDict                       -- b'}'
process' 128 = lift (lift (() <$ getWord8))
process' 133 = tuple1                          -- b'\x85'
process' 134 = tuple2                          -- b'\x86'
process' 135 = tuple3                          -- b'\x87'
process' 136 = true                            -- b'\x88'
process' 137 = false                           -- b'\x89'
process' 140 = utf8lenstr                      -- b'\x8c'
process' 148 = memoize
process' 149 = lift (lift (() <$ getWord64le))         -- TODO: assign frame length
process' n = fail ("invalid load key, " ++ show n ++ ".")

process :: Word8 -> PickM s PyObj
process 46 = stop
process n = traceShow n (process' n >> parsePickle'')  -- handle and continue

parsePickle'' :: PickM s PyObj
parsePickle'' = lift (lift getWord8) >>= process

parsePickle' :: Get PyObj
parsePickle' = runSTT (evalStateT parsePickle'' initialState)

pickleHeader :: Word8 -> Put
pickleHeader = (>>) (putWord8 80) . putWord8

defaultPickleVersion :: Word8
defaultPickleVersion = 4
