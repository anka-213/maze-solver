module Lib
    ( someFunc
    , getMaze
    , solveMazeFile
    , getTinyMaze
    -- , getAStar
    , solveTraceMaze
    , saveMaze
    ) where

import qualified Codec.Picture.Png as Png
import Codec.Picture.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import ExtraInstances ()
import qualified Data.Matrix.Unboxed as Mat
import Data.Matrix.Unboxed (Matrix)
import qualified Data.Vector.Unboxed as VU
import Data.Maybe (fromMaybe)
import Maze
import AStarST

someFunc :: IO ()
someFunc = do
    -- putStrLn "someFunc"
    f <- BS.readFile "img/tiny-maze.png"
    let mat = stringToMaze f
    -- let mat = imageToMatrix img
    -- print $ convMaze mat
    putStrLn $ take 100000 $ prettyMatrix mat
    case drawAStar mat of
        Just mat' -> putStrLn $ take 100000 $ prettyMatrix mat'
        Nothing -> putStrLn "Unsolvable maze"
    -- putStrLn $ processImg f
    pure ()

solveMazeFile :: FilePath -> FilePath -> IO ()
solveMazeFile from to = do
    m <- solveMaze from -- "big-maze.png"
    BL.writeFile to m -- "big-maze-solved.png"

solveTraceMaze :: FilePath -> FilePath -> FilePath -> IO ()
solveTraceMaze from to traceFile = do
    m <- getMaze from
    let traceAct n = saveMaze (traceFile ++ "-" ++ show n ++ ".png")
    x <- runAStarTrace traceAct 100000 m
    let img = mazeToImageBS . drawPath m $ fromMaybe (error "Unsolvable maze") x
    BL.writeFile to img
    pure ()


getMaze :: FilePath -> IO Maze
getMaze fp = stringToMaze <$> BS.readFile fp

saveMaze :: FilePath -> Maze -> IO ()
saveMaze fp = BL.writeFile fp . mazeToImageBS

getTinyMaze :: IO Maze
getTinyMaze = getMaze "img/tiny-maze.png"

-- getAStar :: IO (AStarState Matrix)
-- getAStar = initAstar <$> getTinyMaze

solveMaze :: FilePath -> IO BL.ByteString
solveMaze fp = mazeToImageBS . fromMaybe (error "Unsolvable maze") . drawAStar <$> getMaze fp

imageToMatrix :: (Pixel a, VU.Unbox a) => Image a -> Matrix a
imageToMatrix img = Mat.generate (imageHeight img, imageWidth img) (\(x,y) -> pixelAt img y x)

matrixToImage :: (Pixel a, VU.Unbox a) => Matrix a -> Image a
matrixToImage mat = generateImage (\x y -> mat Mat.! (y,x)) (Mat.cols mat) (Mat.rows mat)

-- getImg :: IO PalettedImage
-- getImg = do
--     f <- BS.readFile "big-maze.png"
--     pure . fst $ either error id $ Png.decodePngWithPaletteAndMetadata f

-- processImg :: BS.ByteString -> Image Pixel8
-- -- processImg :: BS.ByteString -> Matrix MazePixel
-- processImg f = img
--     -- show (meta,pal,img)
--     where
--         Right (palImg, meta) = Png.decodePngWithPaletteAndMetadata f
--         PalettedRGB8 img pal = palImg

stringToMaze :: BS.ByteString -> Matrix MazePixel
stringToMaze = imageToMaze . fst . either error id . Png.decodePngWithPaletteAndMetadata
-- Convert with palette

mazeToImageBS :: Maze -> BL.ByteString
mazeToImageBS = either error id . Png.encodePalettedPng myPalette . mazeToImage

myPalette :: Image PixelRGB8
myPalette = generateImage pixels 4 1
  where
    pixels 0 0 = PixelRGB8 0 0 0       -- Blocked
    pixels 1 0 = PixelRGB8 255 128 128 -- End
    pixels 2 0 = PixelRGB8 255 255 255 -- Free
    pixels 3 0 = PixelRGB8 128 255 128 -- Start
    pixels _ _ = error "Invalid index"
-- myPalette = matrixToImage $ Mat.fromList (1,4)
--     [ PixelRGB8 0 0 0       -- Blocked
--     , PixelRGB8 255 128 128 -- End
--     , PixelRGB8 255 255 255 -- Free
--     , PixelRGB8 128 255 128 -- Start
--     ]

mazeToImage :: Maze -> Image Pixel8
mazeToImage = matrixToImage . Mat.map convPal
    where
        convPal Blocked = 0
        convPal End = 1
        convPal Free = 2
        convPal Start = 3
        convPal Avoid = 3

imageToMaze :: PalettedImage -> Matrix MazePixel
imageToMaze (PalettedRGB8 img pal) = Mat.map (withPaletteRGB8 pal) $ imageToMatrix img
imageToMaze (PalettedRGBA8 img pal) = Mat.map (withPaletteRGBA8 pal) $ imageToMatrix img
imageToMaze _ = error "Unknown image format"

withPaletteRGB8 :: Palette' PixelRGB8 -> Pixel8 -> MazePixel
withPaletteRGB8 pal idx = rgb8ToMaze $ pixelAt (palettedAsImage pal) (fromIntegral idx) 0

-- pixelAt (palettedAsImage pal) 1 0
rgb8ToMaze :: PixelRGB8 -> MazePixel
rgb8ToMaze pix = case pix of
    PixelRGB8 0 0 0 -> Blocked
    PixelRGB8 255 128 128 -> End
    PixelRGB8 255 255 255 -> Free
    PixelRGB8 128 255 128 -> Start
    _ -> error "Unknown color"

withPaletteRGBA8 :: Palette' PixelRGBA8 -> Pixel8 -> MazePixel
withPaletteRGBA8 pal idx = rgba8ToMaze $ pixelAt (palettedAsImage pal) (fromIntegral idx) 0

-- pixelAt (palettedAsImage pal) 1 0
rgba8ToMaze :: PixelRGBA8 -> MazePixel
rgba8ToMaze pix = case pix of
    PixelRGBA8 255 255 255   0 -> Blocked -- Transparent
    PixelRGBA8   0   0   0 255 -> Blocked
    PixelRGBA8 255 128 128 255 -> End
    PixelRGBA8 255 255 255 255 -> Free
    PixelRGBA8 128 255 128 255 -> Start
    _ -> error "Unknown color"