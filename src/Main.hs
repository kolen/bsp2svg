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

readVertices :: LumpEntry -> Get (Seq Vertex)
readVertices lump =
  readArray lump $ Vertex <$> getFloatle <*> getFloatle <*> getFloatle

readLumpContents :: Handle -> LumpEntry -> IO BL.ByteString
readLumpContents handle (LumpEntry offset length) = do
  hSeek handle AbsoluteSeek (fromIntegral offset)
  BL.hGet handle (fromIntegral length)

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  fh <- openFile filename ReadMode
  headerData <- BL.hGetContents fh
  let header = runGet readBspHeader headerData
  let verticesLumpEntry = lumpEntries header `index` 2
  verticesLumpContents <- readLumpContents fh verticesLumpEntry
  let vertices = runGet (readVertices verticesLumpEntry) verticesLumpContents

  print $ Data.Sequence.take 100 vertices
