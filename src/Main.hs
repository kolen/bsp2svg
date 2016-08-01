{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main where

import Data.Sequence (Seq, empty, index, (<|))
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Word
import System.IO
import System.Environment
import Numeric (showHex)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

newtype VertexID   = VertexID   { vertexID   :: Int }
newtype EdgeID     = EdgeID     { edgeID     :: Int }
newtype FaceID     = FaceID     { faceID     :: Int }
newtype FaceEdgeID = FaceEdgeID { faceEdgeId :: Int }

class ID i e | i -> e where
  byID :: BSPMap -> i -> e

instance ID VertexID   Vertex   where byID m (VertexID i)   = vertices m `index` i
instance ID EdgeID     Edge     where byID m (EdgeID i)     = edges m `index` i
instance ID FaceID     Face     where byID m (FaceID i)     = faces m `index` i
instance ID FaceEdgeID FaceEdge where byID m (FaceEdgeID i) = faceEdges m `index` i

data LumpEntry = LumpEntry
  { _offset :: Word32,
    _length :: Word32 }

data BSPHeader = BSPHeader
  { _magic :: Word32,
    _version :: Word32,
    lumpEntries :: Seq LumpEntry }

data Vertex = Vertex Float Float Float
  deriving (Show)

data Edge = Edge VertexID VertexID

data Face = Face
  { firstEdge :: EdgeID,
    numEdges :: Int }

data FaceEdge = FaceEdge EdgeID Bool

data BSPMap = BSPMap
  { bspHeader :: BSPHeader,
    vertices  :: Seq Vertex,
    edges     :: Seq Edge,
    faces     :: Seq Face,
    faceEdges :: Seq FaceEdge }

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
    readArray lump $ Edge <$> (VertexID <$> fromIntegral <$> getWord16le)
                          <*> (VertexID <$> fromIntegral <$> getWord16le)
  lumpIndex = LumpIndex 11

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
            return $ Face (EdgeID $ fromIntegral _firstEdge)
                          (fromIntegral _numEdges)
  lumpIndex = LumpIndex 6

instance LumpData (Seq FaceEdge) where
  readLumpData lump =
    readArray lump readFaceEdge
    where readFaceEdge :: Get FaceEdge
          readFaceEdge = do
            index <- getInt32le
            return $ FaceEdge (EdgeID (abs (fromIntegral index))) (index < 0)
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

faceVertices :: BSPMap -> Int -> [Vertex]
faceVertices bsp faceId =
  foldr combine [] (faceEdgeVertices face)
  where
    combine :: (Vertex, Vertex) -> [Vertex] -> [Vertex]
    combine (v1, v2) [] = [v1, v2]
    combine (v1, _)  l  = v1 : l
    faceEdgeVertices :: Face -> [(Vertex, Vertex)]
    faceEdgeVertices face = toList $ edgeVertices <$> (faceEdges' face)
    faceEdges' :: Face -> Seq FaceEdge
    faceEdges' (Face firstEdge numEdges) =
      S.take numEdges (S.drop (edgeID firstEdge) (faceEdges bsp))
    edgeVertices :: FaceEdge -> (Vertex, Vertex)
    edgeVertices (FaceEdge edgeId reverse) =
      case reverse of
        True  -> (v2, v1)
        False -> (v1, v2)
      where v1 = byID bsp v1id
            v2 = byID bsp v2id
            Edge v1id v2id = byID bsp edgeId
    face :: Face
    face = faces bsp `index` faceId

readBSPMap :: Handle -> IO BSPMap
readBSPMap fh = do
  headerBytes <- BL.hGetContents fh
  let header = runGet readBspHeader headerBytes
  let lumps = lumpEntries header
  vertices  <- readLumpFromFile fh lumps
  edges     <- readLumpFromFile fh lumps
  faces     <- readLumpFromFile fh lumps
  faceEdges <- readLumpFromFile fh lumps
  return $ BSPMap header vertices edges faces faceEdges

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  fh <- openFile filename ReadMode
  bsp <- readBSPMap fh
  print ""
