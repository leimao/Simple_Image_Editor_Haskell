
import PPM
import PPMIO
import Extension

imgOperationMenu :: PPMImage Pixel -> IO ()
imgOperationMenu img = do
    putStrLn "============ Choose Your Action ============"
    putStrLn "Choose Image Operations: "
    putStrLn "(1) Negate R Channel"
    putStrLn "(2) Negate G Channel"
    putStrLn "(3) Negate B Channel"
    putStrLn "(4) Gray Image"
    putStrLn "(5) Edge Detection"
    putStrLn "(6) Sharpen Image"
    putStrLn "(7) Save and Exit"
    putStrLn "(8) Exit without Save"
    optionStr <- getLine 
    let optionInt = read optionStr :: Int 
    processImg img optionInt 

processImg :: PPMImage Pixel -> Int -> IO ()
processImg img option
    | option == 1 = do
        let processedImg = negateRGB R img
        imgOperationMenu processedImg
    | option == 2 = do
        let processedImg = negateRGB G img
        imgOperationMenu processedImg
    | option == 3 = do
        let processedImg = negateRGB B img
        imgOperationMenu processedImg
    | option == 4 = do
        let processedImg = grayImg img
        imgOperationMenu processedImg
    | option == 5 = do
        let processedImg = edgeDetection img
        imgOperationMenu processedImg
    | option == 6 = do
        let processedImg = sharpenImg img
        imgOperationMenu processedImg
    | option == 7 = do
        putStrLn "Input save directory: "
        saveDir <- getLine 
        savePPM saveDir img
        putStrLn "Image Saved Successfully"
        mainMenu
    | option == 8 = do
        mainMenu
    | otherwise = do 
        imgOperationMenu img

imgBlendMenu :: PPMImage Pixel -> IO ()
imgBlendMenu img = do
    putStrLn "Enter in a Fixed PPM to blend: "
    filePath <- getLine
    if filePath == "q" then do
        mainMenu
    else do
        ppmImg <- readPPM filePath
        let blendedImg = blendFixedPPMImages [img, ppmImg]
        savePPM "blended.ppm" blendedImg
        putStrLn "Blended Image Saved Successfully"
        imgBlendMenu blendedImg

optionProcess :: Int -> IO()
optionProcess 1 = do 
    putStrLn "Please Input File Directory: "
    filePath <- getLine
    isPPM <- checkPPM filePath
    if isPPM == False then do
        putStrLn "Invalid Input File"
        return()
    else do
        ppmImg <- readPPM filePath
        imgOperationMenu ppmImg
optionProcess 2 = do
    putStrLn "Enter in a Fixed PPM to blend: "
    filePath <- getLine
    if filePath == "q" then do
        savePPM "blended.ppm" $ getImg memptyFixedPPMImage
        putStrLn "Blended Image Saved Successfully"
        mainMenu
    else do
        ppmImg <- readPPM filePath
        let blendedImg = blendFixedPPMImages [(getImg memptyFixedPPMImage), ppmImg]
        savePPM "blended.ppm" blendedImg
        putStrLn "Blended Image Saved Successfully"
        imgBlendMenu blendedImg
optionProcess _ = do return ()

mainMenu :: IO () 
mainMenu = do 
    putStrLn "============ Choose Your Action ============"
    putStrLn "(1) Read PPM Image "
    putStrLn "(2) Blend Distinct PPM Images "
    putStrLn "(3) Exit "
    optionStr <- getLine 
    let optionInt = read optionStr :: Int 
    optionProcess optionInt

main :: IO ()
main = do
    putStrLn "============================================"
    putStrLn "======= Welcome to the Image Editor ========"
    putStrLn "============================================"
    putStrLn ""
    mainMenu

--edgeDetection 9m30s
--sharpenImg 9m30s