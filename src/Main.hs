{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Sequence
import Data.Word
import System.IO
import System.Environment
import Numeric (showHex)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

data LumpEntry = LumpEntry
  { _offset :: Word32,
    _length :: Word32 }

data BSPHeader = BSPHeader
  { _magic :: Word32,
    _version :: Word32,
    lumpEntries :: Seq LumpEntry }

data Vertex = Vertex Float Float Float
  deriving (Show)

data Edge = Edge Word16 Word16

data BSPMap = BSPMap
  { vertices :: Seq Vertex,
    edges :: Seq Edge}

class LumpData a where
  lumpIndex :: a -> Int
  readLumpData :: LumpEntry -> Get a
  readLumpFromFile :: Handle -> Seq LumpEntry -> IO a
  readLumpFromFile fh entries = do
    bytes <- readLumpBytes fh entry
    return $ runGet (readLumpData entry) bytes
      where entry = entries `index` (lumpIndex (undefined :: a))

instance LumpData (Seq Vertex) where
  readLumpData lump =
    readArray lump $ Vertex <$> getFloatle <*> getFloatle <*> getFloatle
  lumpIndex = const 2

instance LumpData (Seq Edge) where
  readLumpData lump =
    readArray lump $ Edge <$> getWord16le <*> getWord16le
  lumpIndex = const 6

readLumpBytes :: Handle -> LumpEntry -> IO BL.ByteString
readLumpBytes handle (LumpEntry offset length) = do
  hSeek handle AbsoluteSeek (fromIntegral offset)
  BL.hGet handle (fromIntegral length)

readLumpEntry :: Get LumpEntry
readLumpEntry = LumpEntry <$> getWord32le <*> getWord32le

readLumpEntries :: Integer -> Get (Seq LumpEntry)
readLumpEntries count = do
  if count == 0
    then return empty
    else do lump <- readLumpEntry
            rest <- readLumpEntries (count - 1)
            return $ lump <| rest

readBspHeader :: Get BSPHeader
readBspHeader = do
  magic <- getWord32le
  version <- getWord32le
  if magic /= 0x50534249 -- FIXME: use 'IBSP' string
    then error $ showHex magic "Invalid BSP file:"
    else if version /= 38
         then error "Unsupported BSP file version"
         else do lumps <- readLumpEntries 18
                 return $ BSPHeader magic version lumps

readArray :: LumpEntry -> (Get a) -> Get (Seq a)
readArray lumpEntry readItem = do
  let LumpEntry _ length = lumpEntry
  let length64 = fromIntegral length
  bytes <- bytesRead
  if bytes == length64
    then return empty
    else if bytes > length64
    then error "Over-read"
    else do record <- readItem
            rest <- readArray lumpEntry readItem
            return $ record <| rest

main :: IO ()
main = do
  -- args <- getArgs
  -- let filename = head args
  -- fh <- openFile filename ReadMode
  -- headerData <- BL.hGetContents fh
  -- let header = runGet readBspHeader headerData
  -- let verticesLumpEntry = lumpEntries header `index` 2
  -- verticesLumpContents <- readLumpContents fh verticesLumpEntry
  -- let vertices = runGet (readVertices verticesLumpEntry) verticesLumpContents
  -- let edgesLumpEntry = lumpEntries header `index` 11
  -- edgesLumpContents <- readLumpContents fh edgesLumpEntry
  -- let edges = runGet (readEdges edgesLumpEntry) edgesLumpContents

  -- print $ Data.Sequence.take 100 vertices
  print ""
