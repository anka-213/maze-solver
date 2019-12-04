{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
module Maze where

import Data.Matrix.Unboxed (Matrix)
-- import qualified Data.Matrix as Mat
import qualified Data.Matrix.Unboxed as Mat
import Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

type Maze = Matrix MazePixel
data MazePixel = Free | Blocked | Start | End | Avoid | Transparent
    deriving (Eq, Enum)

newtype instance VU.Vector MazePixel = V_MazePixel (VU.Vector Word8)
newtype instance VU.MVector s MazePixel = MV_MazePixel (VU.MVector s Word8)

-- deriving via Int instance VGM.MVector VU.MVector MazePixel
-- deriving via Int instance VG.Vector VU.Vector MazePixel
instance VGM.MVector VU.MVector MazePixel where
    basicLength (MV_MazePixel v) = VGM.basicLength v
    basicUnsafeSlice i j (MV_MazePixel v) = MV_MazePixel (VGM.basicUnsafeSlice i j v)
    basicOverlaps (MV_MazePixel v1) (MV_MazePixel v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = MV_MazePixel <$> VGM.basicUnsafeNew n
    basicInitialize (MV_MazePixel v) = VGM.basicInitialize v
    basicUnsafeRead (MV_MazePixel v) i = toEnum . fromIntegral <$> VGM.basicUnsafeRead v i
    basicUnsafeWrite (MV_MazePixel v) i x = VGM.basicUnsafeWrite v i (fromIntegral $ fromEnum x)

instance VG.Vector VU.Vector MazePixel where
    basicUnsafeFreeze (MV_MazePixel v) = V_MazePixel <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_MazePixel v) = MV_MazePixel <$> VG.basicUnsafeThaw v
    basicLength (V_MazePixel v) = VG.basicLength v
    basicUnsafeSlice i j (V_MazePixel v) = V_MazePixel (VG.basicUnsafeSlice i j v)
    basicUnsafeIndexM (V_MazePixel v) i = toEnum . fromIntegral <$> VG.basicUnsafeIndexM v i

instance VU.Unbox MazePixel

avoidWalls :: Maze -> Maze
avoidWalls maze = Mat.imap avoid maze
    where
    avoid :: Pos -> MazePixel -> MazePixel
    avoid _ Blocked = Blocked
    avoid _ Avoid = Avoid
    avoid pos _ | nearWall = Avoid
        where
            neighs = (maze Mat.!) <$> neighbors 4 (Mat.dim maze) pos
            nearWall = any (\x -> x == Blocked || x == Avoid) neighs
    avoid _ x = x

numFree :: Maze -> Int
numFree = V.length . V.filter (== Free) . Mat.flatten

prettyMatrix :: (Show a, VU.Unbox a) => Matrix a -> String
prettyMatrix m = concat
   [ "┌ ", unwords (replicate ncols blank), " ┐\n"
   , unlines
   [ "│ " ++ unwords (fmap (\j -> fill . show $ m Mat.! (i,j)) [0..ncols-1]) ++ " │" | i <- [0..nrows-1] ]
   , "└ ", unwords (replicate ncols blank), " ┘"
   ]
 where
   (nrows, ncols) = Mat.dim m
   widest = V.maximum $ V.map (length.show) $ Mat.flatten m
   fill str = replicate (widest - length str) ' ' ++ str
   blank = fill ""

instance Show MazePixel where
    show Blocked = "#"
    show Start = "S"
    show End = "E"
    show Free = "_"
    show Avoid = ","
    show Transparent = " "

type Distance = Int
type GVal = Distance
type Pos = (Int, Int)
type Bounds = (Int, Int)

heuristic :: Pos -> Pos -> Distance
heuristic (x1,y1) (x2,y2) = round . (*(10::Double)) . sqrt . fromIntegral $ (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int)

distance :: Pos -> Pos -> Distance
distance = heuristic

neighbors :: Int -> Bounds -> Pos -> [Pos]
neighbors n maze pos = filter (inBounds maze) $ map (add pos. onBoth (*n))
        [(-1,-1), (-1,0), (-1,1)
        ,( 0,-1),         ( 0,1)
        ,( 1,-1), ( 1,0), ( 1,1)
        ]

add :: Pos -> Pos -> Pos
add (r1,c1) (r2,c2) = (r1+r2, c1+c2)

onBoth :: (Int -> Int) -> Pos -> Pos
onBoth f (r,c) = (f r, f c)

inBounds :: Bounds -> Pos -> Bool
inBounds (rMax, cMax) (r,c) = r >= 0 && c >= 0 && r < rMax && c < cMax
