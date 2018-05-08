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

-- Note: "readMaybe" converts a string that contains an Integer into an actual Integer
-- E.g.: "4" ==> Just 4   "17" ==> Just 17. It's a Maybe because the string could potentially 
-- not be a valid string (e.g., "12fa" ==> Nothing "bob" ==> Nothing). 
readInt :: String -> Integer 
readInt str = case readMaybe str :: Maybe(Integer) of 
    Just num -> num 
    Nothing -> error $ "Could not read " ++ str ++ " as an Int!"

{- Read a line of string formatted integers to a list of integers -}
readIntLine :: String -> [Integer]
readIntLine str = map readInt $ words str

intListToPixel :: [Integer] -> Pixel
intListToPixel [r, g, b] = Pixel (r, g, b)
intListToPixel _ = error "Int list to pixel failed"

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n lst = first : (splitEvery n rest) where
    (first, rest) = splitAt n lst

dividePixelIntList :: [Integer] -> [[Integer]]
dividePixelIntList lst = splitEvery 3 lst

dividePixelByWidth :: Integer -> [Pixel] -> [[Pixel]]
dividePixelByWidth w pxs = splitEvery (fromIntegral w) pxs

intLineToPixel :: [Char] -> [Pixel]
intLineToPixel lst = map (intListToPixel) $ dividePixelIntList $ readIntLine lst


pixelToString :: Pixel -> [Char]
pixelToString (Pixel (r, g, b)) = show r ++ " " ++ show g ++ " " ++ show b ++ " "

pixelToStringLine :: [Pixel] -> [Char]
pixelToStringLine pxs = (foldl (\ accum x -> accum ++ pixelToString x) [] pxs) ++ "\n"


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
    pxs = foldl (\ accum line -> accum ++ intLineToPixel line) [] pixelLines

    in PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = pxs}


ppmToString :: PPMImage Pixel -> [Char]
ppmToString (PPMImage {width = w, height = h, magicNumber = mn, maxColor = mc, pixels = pxs}) = let
    headerLines = "P" ++ show mn ++ "\n" ++ show w ++ " " ++ show h ++ "\n" ++ show mc ++ "\n"
    pixelLines = foldl (\ accum x -> accum ++ pixelToStringLine x) [] (dividePixelByWidth w pxs)
    contentLines = headerLines ++ pixelLines

    --contentLines = headerLines ++ (pixelToStringLine pxs)

    in contentLines



checkPPMHeader :: [Char] -> Bool
checkPPMHeader content = if (head mt /= 'P') || (length imgSizes /= 2) || (length mc /= 1) then False else True where
    contentLines = lines content
    headerLines = take 3 contentLines
    mt = headerLines !! 0
    imgSizes = words (headerLines !! 1)
    mc = words (headerLines !! 2)



checkPPM :: [Char] -> IO Bool
checkPPM filePath = do 
    content <- readFile filePath
    if checkPPMHeader content == True then return True else return False

readPPM :: [Char] -> IO (PPMImage Pixel)
readPPM filePath = do
    content <- readFile filePath
    let img = stringToPPM content
    --putStrLn $ show img
    return img


savePPM :: [Char] -> PPMImage Pixel -> IO ()
savePPM filePath img = do
    let content = ppmToString img
    writeFile filePath content



{-
intLineToPixel :: [Int] -> [Pixel]
intLineToPixel lst = map

parsePPM :: [Char] -> PPMImage Pixel
parsePPM content = let
    contentLines = lines content
    headerLines = take 3 contentLines
    pixelLines = drop 3 contentLines
    magicNumber = readInt (tail (headerLines !! 0))
    imgSizes = readIntLine (headerLines !! 1)
    w = imgSizes !! 0
    h = imgSizes !! 1
    mc = readInt (headerLines !! 2)
-}



testCropImage1 = PPMImage 2 2 3 255 [(Pixel (-255,500,255)),(Pixel (-255,500,255)),(Pixel (255,128,-255)),(Pixel (255,0,-255))]


testImage1 = PPMImage 2 2 3 255 [(Pixel (255,0,255)),(Pixel (255,0,255)),(Pixel (255,0,255)),(Pixel (255,0,255))]

testImage2 = PPMImage 2 3 3 255 [(Pixel (255,255,255)),(Pixel (255,0,255)),(Pixel (255,0,255)),(Pixel (255,0,255)), (Pixel (128,128,128)),(Pixel (128,128,128))]

testPxs = [(Pixel (1,2,3)), (Pixel (2,3,4)), (Pixel (3,4,5)), (Pixel (1,2,3)), (Pixel (2,3,4)), (Pixel (3,4,5))]
{-
P3
4 4
255
0  0  0   100 0  0       0  0  0    255   0 255
0  0  0    0 255 175     0  0  0     0    0  0
0  0  0    0  0  0       0 15 175    0    0  0
255 0 255  0  0  0       0  0  0    255  255 255
-}
{-
parsePPM :: [Char] -> PPMImage Pixel
parsePPM content = let
    contentLines = lines content
    headerLines = take 3 contentLines
    pixelLines = drop 3 contentLines
    magicNumber = readInt (tail (headerLines !! 0))
    readIntLine (headerLines !! 1)





    contentLines1 = lines content
    magicNumber = readInt (tail (head contentLines1))
    contentLines2 = tail contentLines1




readPPM :: [Char] -> IO (PPMImage Pixel)
readPPM filepath = do
    content <- readFile filepath
    
-}