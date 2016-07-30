module Main where

import Data.Sequence
import Data.Word
import System.IO
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

data LumpEntry = LumpEntry
  { _offset :: Word32,
    _length :: Word32 }

data BSPHeader = BSPHeader
  { _magic :: Word32,
    _version :: Word32,
    _lumps :: Seq LumpEntry }

data Vertex = Vertex Float Float Float

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
  if magic /= 0x49425350
    then error "Invalid BSP file"
    else if version /= 38
         then error "Unsupported BSP file version"
         else do lumps <- readLumpEntries 18
                 return $ BSPHeader magic version lumps

readVertices :: LumpEntry -> Get (Seq Vertex)
readVertices (LumpEntry _ length) =
  replicateM numVertices $ Vertex <$> getFloatle <*> getFloatle <*> getFloatle
  where numVertices = (fromIntegral length) `quot` (3 * 4)

readLumpContents :: Handle -> LumpEntry -> IO BL.ByteString
readLumpContents handle (LumpEntry offset length) = do
  hSeek handle AbsoluteSeek (fromIntegral offset)
  BL.hGet handle (fromIntegral length)

main :: IO ()
main = do
  putStrLn "hello world"
