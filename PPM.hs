module PPM 
(
    Pixel(..),
    PPMImage(..),
    Channel(..),
    Kernel(..),
    negateRGB,
    grayImg,
    convImg,
    edgeDetection,
    sharpenImg,
    cropPixelImg,
) where

import Data.List


newtype Pixel = Pixel (Integer, Integer, Integer)
    deriving (Eq, Show)

{- Pixel algorithmics -}
instance Num (Pixel) where
    Pixel (r, g, b) + Pixel (r', g', b') = Pixel (r + r', g + g', b + b')
    Pixel (r, g, b) - Pixel (r', g', b') = Pixel (r - r', g - g', b - b')
    Pixel (r, g, b) * Pixel (r', g', b') = Pixel (r * r', g * g', b * b')
    abs (Pixel (r, g, b)) = Pixel (abs r, abs g, abs b)
    signum (Pixel (r, g, b)) = Pixel (signum r, signum g, signum b)
    fromInteger i = Pixel (fromInteger i, fromInteger i, fromInteger i)


data PPMImage a = PPMImage {width :: Integer, height:: Integer, magicNumber :: Integer, maxColor :: Integer, pixels :: [a]}
    deriving (Eq, Show)


instance Functor PPMImage where
    -- fmap :: (a -> b) -> PPMImage a -> PPMImage b
    fmap f (PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = pxs}) = PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = (map f pxs)}


data Channel = R | G | B
    deriving (Eq, Show)

data Kernel = Kernel [Integer]
    deriving (Eq, Show)

{- Negate single pixel -}
negatePixel :: Channel -> Integer -> Pixel -> Pixel
negatePixel channel mc (Pixel (r, g, b))
    | channel == R = Pixel (mc - r, g, b)
    | channel == G = Pixel (r, mc - g, b)
    | channel == B = Pixel (r, g, mc - b)
    | otherwise = error "Unknown channel"

{- Gray single pixel -}
grayPixel :: Pixel -> Pixel
grayPixel (Pixel (r, g, b)) = Pixel (ave, ave, ave) where 
    ave = (r + g + b) `div` 3

{- Negate all pixels in PPMImage Pixel -}
negateRGB :: Channel -> PPMImage Pixel -> PPMImage Pixel
negateRGB channel img@(PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = pxs}) = fmap func img where
    func = negatePixel channel mc

{- Gray all pixels in PPMImage Pixel -}
grayImg :: PPMImage Pixel -> PPMImage Pixel
grayImg img = fmap grayPixel img


{- Slice a list and get range -}
sliceList :: Int -> Int -> [a] -> [a]
sliceList start end lst = take (end - start) (drop start lst)

{- Get list elements by index -}
getListByIndex :: [a] -> [Int] -> [a]
getListByIndex lst idx = map (lst !!) idx

{- Get pixels of a specific row in PPMImage Pixel -}
getImgRow :: Integer -> PPMImage Pixel -> [Pixel]
getImgRow n img@(PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = pxs}) = sliceList (fromIntegral (n * w)) (fromIntegral ((n + 1) * w)) pxs

{- Pad PPMImage Pixel by specific number of empty pixels -}
padImg :: Integer -> PPMImage Pixel -> PPMImage Pixel
padImg n img@(PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = pxs}) = PPMImage {width = w + 2 * n, height = h + 2 * n, magicNumber = mn, maxColor = mc, pixels = padded_pxs} where
    padded_pxs = (replicate (fromIntegral ((w + 2 * n) * n - n)) (Pixel (0, 0, 0))) ++ foldl (\ accum x -> accum ++ replicate (fromIntegral (2 * n)) (Pixel (0, 0, 0)) ++ getImgRow x img) [] [0 .. (h - 1)] ++ (replicate (fromIntegral ((w + 2 * n) * n + n)) (Pixel (0, 0, 0)))


{- Calculate the valid pixel index after padding -}
{-
n: number of padding layers
w: width of image before padding
h: height of image before padding
-}
validPixelIndex :: Int -> Int -> Int -> [Int]
validPixelIndex n w h = foldl (\ accum x -> accum ++ sliceList (x * (w + 2 * n) + n) (x * (w + 2 * n) + n + w) idx) [] [n .. (h + n - 1)] where 
    idx = [0 .. ((w + 2 * n) * (h + 2 * n) - 1)]

{- Calculate the pixel index of receptive field -}
{- Not for the index at the boarder -}
{-
n: number of padding layers
w: width of image after padding
i: index of center pixel
-}
getReceptiveIndex :: Int -> Int -> Int -> [Int]
getReceptiveIndex n w i = foldl (\ accum x -> accum ++ (getReceptiveRow n x)) [] [(i - n * w), (i - n * w + w) .. (i + n * w)]
    where getReceptiveRow n k = [(k - n) .. (k + n)]

{- Rotate the kernel by 180 degree -}
flipKernel :: Kernel -> Kernel
flipKernel (Kernel lst) = Kernel (reverse lst)

{- Crop single pixel using maximumColor -}
cropPixel :: Integer -> Pixel -> Pixel
cropPixel mc (Pixel (r, g, b)) = Pixel ((crop r), (crop g), (crop b)) where
    crop v = maximum ([0, (minimum [mc, v])])

{- Pixel addition -}
pixelAdd :: Pixel -> Pixel -> Pixel
pixelAdd (Pixel (r, g, b)) (Pixel (r', g', b')) = Pixel (r + r', g + g', b + b')

{- Pixel multiplication -}
pixelMutiply :: Pixel -> Integer -> Pixel
pixelMutiply (Pixel (r, g, b)) n = Pixel (r * n, g * n, b * n)

{- Pixel kernel interaction -}
pixelKernelInterat :: Kernel -> [Pixel] -> Pixel
pixelKernelInterat (Kernel lst) pxs = sum (zipWith (pixelMutiply) pxs lst)

{- Convolution using kernel for PPMImage Pixel -}
convImg :: PPMImage Pixel -> Integer -> Kernel -> PPMImage Pixel
convImg img@(PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = pxs}) n kernel@(Kernel lst) = let 
    paddedImg = padImg n img
    padded_w = w + 2 * n
    padded_h = h + 2 * n
    validIndex = validPixelIndex (fromIntegral n) (fromIntegral w) (fromIntegral h)
    flippedKernel = flipKernel kernel
    receptiveIndices = map (getReceptiveIndex (fromIntegral n) (fromIntegral padded_w)) validIndex
    receptivePixels = map (getListByIndex (pixels paddedImg)) receptiveIndices
    conv_pxs = map (pixelKernelInterat flippedKernel) receptivePixels
    in 
    PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = conv_pxs}

{- Crop all pixels in PPMImage Pixel using maximumColor -}
cropPixelImg :: PPMImage Pixel -> PPMImage Pixel
cropPixelImg img@(PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = pxs}) = fmap func img where 
    func = cropPixel mc

{- PPMImage Pixel edge detection -}
edgeDetection :: PPMImage Pixel -> PPMImage Pixel
edgeDetection img = cropPixelImg $ convImg img (kernelN edgeDetectionKernel) edgeDetectionKernel

{- PPMImage Pixel sharpening -}
sharpenImg :: PPMImage Pixel -> PPMImage Pixel
sharpenImg img = cropPixelImg $ convImg img (kernelN shapenKernel) shapenKernel

{- Number of lines for padding using the kernel -}
kernelN :: Kernel -> Integer
kernelN (Kernel lst) = ((round (sqrt (fromIntegral (length lst)))) - 1) `div` 2


shapenKernel = Kernel [0, -1, 0, -1, 5, -1, 0, -1, 0]
edgeDetectionKernel = Kernel [-1, -1, -1, -1, 8, -1, -1, -1, -1]

{-Some test cases -}
{-
exampleImage = PPMImage 3 3 3 255 [(Pixel (1,1,1)),(Pixel (2,2,2)),(Pixel (3,3,3)),(Pixel (4,4,4)), (Pixel (5,5,5)), (Pixel (6,6,6)), (Pixel (7,7,7)), (Pixel (8,8,8)), (Pixel (9,9,9))]

testKernel = Kernel [-1, -2, -1, 0, 0, 0, 1, 2, 1]

shapenKernel = Kernel [0, -1, 0, -1, 5, -1, 0, -1, 0]
edgeDetectionKernel = Kernel [-1, -1, -1, -1, 8, -1, -1, -1, -1]

testCropImage1 = PPMImage 2 2 3 255 [(Pixel (-255,500,255)),(Pixel (-255,500,255)),(Pixel (255,128,-255)),(Pixel (255,0,-255))]

testImage1 = PPMImage 2 2 3 255 [(Pixel (255,0,255)),(Pixel (255,0,255)),(Pixel (255,0,255)),(Pixel (255,0,255))]

testImage2 = PPMImage 2 3 3 255 [(Pixel (255,255,255)),(Pixel (255,0,255)),(Pixel (255,0,255)),(Pixel (255,0,255)), (Pixel (128,128,128)),(Pixel (128,128,128))]

pxs = [(Pixel (1,2,3)), (Pixel (2,3,4)), (Pixel (3,4,5)), (Pixel (1,2,3)), (Pixel (2,3,4)), (Pixel (3,4,5))]
kn = Kernel [1, 2, 3]
-}
