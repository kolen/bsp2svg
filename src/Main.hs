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

data Face = Face
  { firstEdge :: Int,
    numEdges :: Int }

data FaceEdge = FaceEdge Int Bool

data BSPMap = BSPMap
  { vertices :: Seq Vertex,
    edges :: Seq Edge}

newtype LumpIndex a = LumpIndex Int

class LumpData a where
  lumpIndex :: LumpIndex a
  readLumpData :: LumpEntry -> Get a
  readLumpFromFile :: Handle -> Seq LumpEntry -> IO a
  readLumpFromFile fh entries = do
    bytes <- readLumpBytes fh entry
    return $ runGet (readLumpData entry) bytes
      where (LumpIndex li) = lumpIndex :: LumpIndex a
            entry = entries `index` li

instance LumpData (Seq Vertex) where
  readLumpData lump =
    readArray lump $ Vertex <$> getFloatle <*> getFloatle <*> getFloatle
  lumpIndex = LumpIndex 2

instance LumpData (Seq Edge) where
  readLumpData lump =
    readArray lump $ Edge <$> getWord16le <*> getWord16le
  lumpIndex = LumpIndex 6

instance LumpData (Seq Face) where
  readLumpData lump =
    readArray lump readFace
    where readFace :: Get Face
          readFace = do
            getWord16le -- plane
            getWord16le -- plane_side
            _firstEdge <- getWord32le
            _numEdges <- getWord16le
            getWord16le -- texture_info
            skip 4 -- lightmap_syles
            getWord32le -- lightmap_offset
            return $ Face (fromIntegral _firstEdge) (fromIntegral _numEdges)
  lumpIndex = LumpIndex 6

instance LumpData (Seq FaceEdge) where
  readLumpData lump =
    readArray lump readFaceEdge
    where readFaceEdge :: Get FaceEdge
          readFaceEdge = do
            index <- getInt32le
            return $ FaceEdge (abs (fromIntegral index)) (index < 0)
  lumpIndex = LumpIndex 12

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
