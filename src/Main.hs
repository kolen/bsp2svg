module Main where

import Data.Sequence
import Data.Word
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

data Lump = Lump
  { _offset :: Word32,
    _length :: Word32 }

data BSPHeader = BSPHeader
  { _magic :: Word32,
    _version :: Word32,
    _lumps :: Seq Lump }

data Vertex = Vertex Float Float Float

readLump :: Get Lump
readLump = Lump <$> getWord32le <*> getWord32le

readLumps :: Integer -> Get (Seq Lump)
readLumps count = do
  if count == 0
    then return empty
    else do lump <- readLump
            rest <- readLumps (count - 1)
            return $ lump <| rest

readBspHeader :: Get BSPHeader
readBspHeader = do
  magic <- getWord32le
  version <- getWord32le
  if magic /= 0x49425350
    then error "Invalid BSP file"
    else if version /= 38
         then error "Unsupported BSP file version"
         else do lumps <- readLumps 18
                 return $ BSPHeader magic version lumps

readVertices :: Lump -> Get (Seq Vertex)
readVertices (Lump _ length) =
  replicateM numVertices $ Vertex <$> getFloatle <*> getFloatle <*> getFloatle
  where numVertices = (fromIntegral length) `quot` (3 * 4)

main :: IO ()
main = do
  putStrLn "hello world"
