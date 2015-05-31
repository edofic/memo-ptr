{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

module Control.Memo 
( newCache
, withCache
, withCacheIO
) where

import System.Mem.StableName
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.Ord

import qualified Data.Map as Map

data Call = Call {-# UNPACK #-} !(StableName ())
                 {-# UNPACK #-} !(StableName ()) 
                 deriving Eq

call :: (a -> b) -> a -> Call
call !f !a = unsafePerformIO $ do
  nf <- makeStableName f
  na <- makeStableName a
  return $ Call (unsafeCoerce nf) (unsafeCoerce na)

instance Ord Call where
  Call f1 a1 `compare` Call f2 a2 = 
    case comparing hashStableName f1 f2 of
      EQ    -> comparing hashStableName a1 a2 
      other -> other

data Value where
  Value :: a -> Value

newtype Cache = Cache (IORef (Map.Map Call Value))

newCache :: IO Cache 
newCache = Cache `fmap` newIORef Map.empty

withCacheIO :: Cache -> (a -> b) -> a -> IO b
withCacheIO (Cache ref) f a = do 
  cache <- readIORef ref 
  case Map.lookup (call f a) cache of
    Just (Value b) -> return $ unsafeCoerce b
    Nothing -> do 
      let res = f a
      modifyIORef' ref $ Map.insert (call f a) (Value res)
      return res

withCache :: Cache -> (a -> b) -> a -> b
withCache ref f a = unsafePerformIO $ withCacheIO ref f a

