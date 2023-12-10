module Data.Python.Pickle(unpickleFile, parsePickle) where

import Data.Binary(decodeFile)
import Data.Python.Pickle.Class(TryFromPy, Pickled(unpickle), parsePickle)

unpickleFile :: TryFromPy a => FilePath -> IO a
unpickleFile = fmap unpickle . decodeFile
