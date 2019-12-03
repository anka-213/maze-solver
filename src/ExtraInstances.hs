{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}
module ExtraInstances where

-- import qualified Codec.Picture.Png as Png
import Codec.Picture.Types
import Data.Word (Word8)

deriving instance Show PalettedImage
deriving instance Show (Image Word8)
deriving instance Show (Palette' Pixel8)
deriving instance Show (Palette' PixelRGB8)
deriving instance Show (Palette' PixelRGBA8)
deriving instance Show (Palette' PixelRGB16)
deriving instance Show DynamicImage
deriving instance Show (Image Pixel16)
deriving instance Show (Image Pixel32)
deriving instance Show (Image PixelF)
deriving instance Show (Image PixelYA8)
deriving instance Show (Image PixelYA16)
deriving instance Show (Image PixelRGB8)
deriving instance Show (Image PixelRGB16)
deriving instance Show (Image PixelRGBF)
deriving instance Show (Image PixelRGBA8)
deriving instance Show (Image PixelRGBA16)
deriving instance Show (Image PixelYCbCr8)
deriving instance Show (Image PixelCMYK8)
deriving instance Show (Image PixelCMYK16)
