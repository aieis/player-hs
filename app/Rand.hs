module Rand where

import Data.ByteString
import Data.ByteString.Internal
import Data.Word8
import System.Random
import Foreign.Ptr
import GHC.ForeignPtr
  
randomBytes :: Int -> StdGen -> [Word8]
randomBytes 0 _ = []
randomBytes n g = fromIntegral value : randomBytes (n - 1) nextG
  where (value, nextG) = next g

randomByteString :: Int -> IO (Ptr Word8)
randomByteString n =  do
  g <- getStdGen
  let r = pack $ randomBytes n g
  let (ptr, c) = toForeignPtr0 r

  return (unsafeForeignPtrToPtr ptr)
