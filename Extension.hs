module Extension
(
    FixedPPMImage(..),
    blendFixedPPMImages,
    memptyFixedPPMImage,
) where

import PPM

newtype FixedPPMImage = FixedPPMImage {getImg:: PPMImage Pixel}
    deriving (Eq, Show)

{- Monoid instance for FixedPPMImage -}
instance Monoid FixedPPMImage where
    mempty = memptyFixedPPMImage
    FixedPPMImage x@(PPMImage {width = _, height = _, magicNumber = _, maxColor = _, pixels = pxs_x}) `mappend` FixedPPMImage y@(PPMImage {width = _, height = _, magicNumber = _, maxColor = _, pixels = pxs_y}) = 
        FixedPPMImage (cropPixelImg (PPMImage {width = 10, height = 10, magicNumber = 3, maxColor = 255, pixels = pxs})) where
            pxs = zipWith (+) pxs_x pxs_y

{-
A valid "FixedPPMImage" is defined by fixed constraints on the meta-data of its wrapped image. The wrapped image must have its meta-data values be equal to the following:
width = 10
height = 10
magicNumber = 3
maxColor = 255
Based on the width and height the total number of pixels is 100
-}

{- Verify if the PPMImage Pixel satisfy FixedPPMImage criteria -}
verifyFixedPPMImage :: PPMImage Pixel -> Bool
verifyFixedPPMImage img@(PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = pxs}) = 
    if (w == 10) && (h == 10) && (mn == 3) && (mc == 255) && (length pxs == 10 * 10) then True else False

{- Verify if all the elements in the list of PPMImage Pixel satisfy FixedPPMImage criteria -}
verifyFixedPPMImageList :: [PPMImage Pixel] -> Bool
verifyFixedPPMImageList imgs = foldl (\ accum img -> accum && (verifyFixedPPMImage img)) True imgs

{- Mempty for FixedPPMImage -}
memptyFixedPPMImage :: FixedPPMImage
memptyFixedPPMImage = FixedPPMImage (PPMImage {width = 10, height = 10, magicNumber = 3, maxColor = 255, pixels = pxs}) where
    pxs = replicate (10  * 10) (Pixel (0, 0, 0))

{- Blend a list of PPMImage Pixel which satisfy FixedPPMImages to single PPMImage Pixel -}
blendFixedPPMImages :: [PPMImage Pixel] -> PPMImage Pixel
blendFixedPPMImages imgs = if verifyFixedPPMImageList imgs then getImg.mconcat.map FixedPPMImage $ imgs else error "PPMImages are not FixedPPMImages."


{- Test cases -}
{-
ppmTest1 = PPMImage 2 2 3 255 [(Pixel (255,18,9)),(Pixel (255,255,0)),(Pixel (255,5,10)),(Pixel (255,255,0))]

ppmTest2 = PPMImage 2 2 3 255 [(Pixel (0,0,0)),(Pixel (255,0,0)),(Pixel (23,123,8)),(Pixel (15,125,75))]
-}

