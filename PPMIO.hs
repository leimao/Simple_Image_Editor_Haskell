module PPMIO
(   
    checkPPM,
    readPPM,
    savePPM,
    readInt,
) where

import System.IO
import Data.List
import Text.Read -- "readMaybe" is defined in this module 

import PPM

{- Read string to integer -}
readInt :: String -> Integer 
readInt str = case readMaybe str :: Maybe(Integer) of 
    Just num -> num 
    Nothing -> error $ "Could not read " ++ str ++ " as an Int!"

{- Read a line of string formatted integers to a list of integers -}
readIntLine :: String -> [Integer]
readIntLine str = map readInt $ words str

{- Make a list of three integers to pixel -}
intListToPixel :: [Integer] -> Pixel
intListToPixel [r, g, b] = Pixel (r, g, b)
intListToPixel _ = error "Int list to pixel failed"

{- Split list -}
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n lst = first : (splitEvery n rest) where
    (first, rest) = splitAt n lst

{- Divide list into sublists for every three elements -}
dividePixelIntList :: [Integer] -> [[Integer]]
dividePixelIntList lst = splitEvery 3 lst

{- Divide list into sublists by width -}
dividePixelByWidth :: Integer -> [Pixel] -> [[Pixel]]
dividePixelByWidth w pxs = splitEvery (fromIntegral w) pxs

{- Turn a line of characters to a list of pixels -}
intLineToPixel :: [Char] -> [Pixel]
intLineToPixel lst = map (intListToPixel) $ dividePixelIntList $ readIntLine lst

{- Turn pixel to strings for storage -}
pixelToString :: Pixel -> [Char]
pixelToString (Pixel (r, g, b)) = show r ++ " " ++ show g ++ " " ++ show b ++ " "

{- Turn a list of pixels to strings for storage -}
pixelToStringLine :: [Pixel] -> [Char]
pixelToStringLine pxs = (foldl (\ accum x -> accum ++ pixelToString x) [] pxs) ++ "\n"


{- Turn PPM formatted string to PPMImage Pixel -}
stringToPPM :: [Char] -> PPMImage Pixel
stringToPPM content = let
    contentLines = lines content
    headerLines = take 3 contentLines
    pixelLines = drop 3 contentLines
    mn = readInt (tail (headerLines !! 0))
    imgSizes = readIntLine (headerLines !! 1)
    w = imgSizes !! 0
    h = imgSizes !! 1
    mc = readInt (headerLines !! 2)
    --pxs = foldl (\ accum line -> accum ++ intLineToPixel line) [] pixelLines
    pxs = intLineToPixel $ foldl (\ accum line -> accum ++ " " ++ line) [] pixelLines

    in PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = pxs}

{- Turn PPMImage Pixel to PPM formatted string -}
ppmToString :: PPMImage Pixel -> [Char]
ppmToString (PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = pxs}) = let
    headerLines = "P" ++ show mn ++ "\n" ++ show w ++ " " ++ show h ++ "\n" ++ show mc ++ "\n"
    pixelLines = foldl (\ accum x -> accum ++ pixelToStringLine x) [] (dividePixelByWidth w pxs)
    contentLines = headerLines ++ pixelLines

    in contentLines

{- Check if the read content header is PPM header -}
checkPPMHeader :: [Char] -> Bool
checkPPMHeader content = if (head mt /= 'P') || (length imgSizes /= 2) || (length mc /= 1) then False else True where
    contentLines = lines content
    headerLines = take 3 contentLines
    mt = headerLines !! 0
    imgSizes = words (headerLines !! 1)
    mc = words (headerLines !! 2)

{- Check if the file is a PPM image -}
checkPPM :: [Char] -> IO Bool
checkPPM filePath = do 
    content <- readFile filePath
    if checkPPMHeader content == True then return True else return False

{- Check read the file to a PPMImage Pixel -}
readPPM :: [Char] -> IO (PPMImage Pixel)
readPPM filePath = do
    content <- readFile filePath
    let img = stringToPPM content
    return img

{- Save PPMImage Pixel to file -}
savePPM :: [Char] -> PPMImage Pixel -> IO ()
savePPM filePath img = do
    let content = ppmToString img
    writeFile filePath content


{- Some test cases -}
{-
testCropImage1 = PPMImage 2 2 3 255 [(Pixel (-255,500,255)),(Pixel (-255,500,255)),(Pixel (255,128,-255)),(Pixel (255,0,-255))]

testImage1 = PPMImage 2 2 3 255 [(Pixel (255,0,255)),(Pixel (255,0,255)),(Pixel (255,0,255)),(Pixel (255,0,255))]

testImage2 = PPMImage 2 3 3 255 [(Pixel (255,255,255)),(Pixel (255,0,255)),(Pixel (255,0,255)),(Pixel (255,0,255)), (Pixel (128,128,128)),(Pixel (128,128,128))]

testPxs = [(Pixel (1,2,3)), (Pixel (2,3,4)), (Pixel (3,4,5)), (Pixel (1,2,3)), (Pixel (2,3,4)), (Pixel (3,4,5))]
-}