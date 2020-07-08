module Main where

import Data.Char
import Data.Binary
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.Vector.Storable as V
import qualified Foreign.Storable as ST
import qualified Data.ByteString as BS
import qualified Graphics.Netpbm as NP

-- Store functions

data Store s a = Store s (s -> a)

seek :: s -> Store s a -> Store s a
seek s (Store _ a) = Store s a

pos :: Store s a -> s
pos (Store s a) = s

peek :: s -> Store s a -> a
peek s (Store _ a) = a s

seeks :: (s -> s) -> Store s a -> Store s a
seeks fs (Store s a) = Store (fs s) a

peeks :: (s -> s) -> Store s a -> a
peeks f (Store s a) = a $ f s

current :: Store s a -> a
current (Store s a) = a s

instance Functor (Store s) where
    fmap f (Store s fs) = Store s (f . fs)


-- Image

data Coord = Coord Int Int
data RGB = RGB Word8 Word8 Word8

instance Eq Coord where
    (Coord a b) == (Coord x y) = a == x && b == y

instance Eq RGB where
    (RGB r g b) == (RGB r2 g2 b2) = r == r2 && g == g2 && b == b2



applyFilter :: (Store Coord RGB -> Store Coord RGB) -> NP.PPM -> NP.PPM
applyFilter filt ppm@(NP.PPM header@(NP.PPMHeader t w h) img) = 
        let 
            store = ppmToStore ppm
            filtered = filt store


            ppmToStore :: NP.PPM -> Store Coord RGB
            ppmToStore (NP.PPM (NP.PPMHeader _ w h) img) = 
                    let pixels = toPixel $ NP.pixelDataToIntList img
                        imgf (Coord row col) = 
                            if w * h <= row * w + col 
                                then pixels !! (row * w + col)
                                else  RGB 0 0 0
                    in Store (Coord 0 0) $ imgf


            -- | [R, G, B, R, G, ...] -> [RGB, RGB, ...]
            toPixel :: [Int] -> [RGB]
            toPixel [] = []
            toPixel (r:g:b:px) = 
                let 
                    red    = fromIntegral r
                    green  = fromIntegral g
                    blue   = fromIntegral b
                in (RGB red green blue):(toPixel px)


            storeToppm :: Store Coord RGB -> NP.PPM
            storeToppm st = 
                    let 
                        rgb8List = toList $ seek (Coord 0 0) st
                        imgData = NP.PpmPixelDataRGB8 $ V.fromList rgb8List
                    in NP.PPM header imgData


            toList :: Store Coord RGB -> [NP.PpmPixelRGB8]
            toList st@(Store (Coord row col) img) =
                    let newCoord = newCoordf (row, col)
                    in if newCoord == (Coord (-1) (-1))
                        then 
                             []
                        else
                            let (RGB r g b) = current st
                                pixel = (NP.PpmPixelRGB8 r g b)
                            in pixel:(toList (Store newCoord img))


            newCoordf (row, col)
                    | h == row && w == col = (Coord (-1) (-1))
                    | h == row             = (Coord row (col+1))
                    | w == col             = (Coord (row+1) 0)


        in storeToppm filtered


-- kernel :: [[]] -> Store Coord RGB -> Store Coord RGB
-- kernel filt st = 
--         foldl () 0 $ zip 


-- IO  
main = 
        do
            result <- reader
            (image:_) <- handlePPM result
            printStatus result
            writeFile "WriteTo.ppm" $ serialize image
            return ()
    

reader :: IO NP.PpmParseResult
reader = do
            file <- BS.readFile "Sample.ppm"
            return $ NP.parsePPM file


serialize :: NP.PPM -> String
serialize (NP.PPM (NP.PPMHeader t w h) img)  =
                let intList = NP.pixelDataToIntList img 
                    headStr = unlines [show (maximum intList), show h, show w, "P3"]
                in headStr ++ (unlines $ map show intList)


handlePPM :: NP.PpmParseResult -> IO [NP.PPM]
handlePPM (Right (images, rest)) = return images
handlePPM (Left err) = 
        do
            putStrLn "Failed to parse PPM image:"
            exitFailure


printStatus :: NP.PpmParseResult -> IO ()
printStatus (Right file) =
        let result = case file of
                ([], _)           -> ""
                (images, Nothing) -> concat $ map show images
                (_, Just _)       -> show file
        in print result
printStatus (Left err) =
        do
            putStrLn "Failed to get status PPM image:"
            putStrLn err
            exitFailure


{--
dminuoso 15:22:46
Func: Interesting idea, but there's a problem with that. If you encounter RGB 0 0 0 pixels in your valid data, that won't work.
Furthermore, you only scan one single column/row with that.
Essentially you can't convert it back, without knowing the width and height.
← Bergle_2 has left (Ping timeout: 246 seconds)
→ polyphem has joined
→ manjaro-user_ has joined
→ Bergle_1 has joined
→ merijn has joined
← wei2912 has left (Quit: Lost terminal)
→ pfurla has joined
← merijn has left (Ping timeout: 256 seconds)
dminuoso 15:41:00
Func: Let's approach this differently. Our task is to take an image, convert some filter to it, and produce the image back.
dminuoso 15:43:29
Operationally that would take NP.PPM, convert it into the Store representation, run some as-of-yet unspecified code on the Store representation, and convert it back to NP.PPM
The unspecified code on the Store representation will be some `gaussianFilter :: Store Coord RGBA -> Store Coord RGBA`
So the entire function would be some `applyFilter :: (Store Coord RGBA -> Store Coord RGBA) -> NP.PPM -> NP.PPM`
You should be able to implement that particular function already.
(And it should make it more obvious how do deal with width and height)
The ergonomics of this interface will be `let newImage = applyFilter gaussianBlur someImage in ...`


Func: Ill give you some bits to get started. Recall how an image convolution (the process by which we apply a kernel to each pixel) worked. Try encoding how the sharpen (i.e. a [[0,-1,0], [-1, 5, -1], [0,-1,0]] kernel) would look like for a single pixel.
Func: Ill give you some bits to get started. Recall how an image convolution (the process by which we apply a kernel to each pixel) worked. Try encoding how the sharpen (i.e. a [[0,-1,0], [-1, 5, -1], [0,-1,0]] kernel) would work for a pixel using a function.
The signature for such a function would be `Store Coord RGB -> RGB`, where the "current position" of the argument image is the pixel we want to recalculate, and the result is the newly calculated pixel.
Make sure you look at the functions you wrote for Store at the beginning, you will need some of them.

Func: You'll be missing one final part after that, which is the `convolute :: Store Coord RGB -> (Store Coord RGB -> RGB) -> Store Coord RGB`
Once you have that, you can plump everything together and give it a run! :)
The convolute is the real magic part, its implementation is not very complicated but subtle. :)
Or rather, call it `convolve` I guess.
--}
