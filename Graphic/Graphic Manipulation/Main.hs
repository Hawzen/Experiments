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

instance Eq RGB where
    (RGB r g b) == (RGB r2 g2 b2) = r == r2 && g == g2 && b == b2


-- | [R, G, B, R, G, ...] -> [RGB, RGB, ...]
toPixel :: [Int] -> [RGB]
toPixel [] = []
toPixel (r:g:b:px) = let 
                         red    = fromIntegral r
                         green  = fromIntegral g
                         blue   = fromIntegral b
                     in (RGB red green blue):(toPixel px)


ppmToStore :: NP.PPM -> Store Coord RGB
ppmToStore (NP.PPM (NP.PPMHeader _ w c) img) = 
        let pixels = toPixel $ NP.pixelDataToIntList img 
        in Store (Coord 0 0) $ imgf pixels
            where
                imgf :: [RGB] -> Coord -> RGB
                imgf pixels (Coord row col) 
                    | (w*c) <= row * w + col = pixels !! (row * w + col)
                    | otherwise = RGB 0 0 0


storeToppm :: Store Coord RGB -> NP.PPM
storeToppm st = let 
                 imgData = NP.PpmPixelDataRGB8 $ V.fromList $ toList (seek (Coord 0 0) st)
                 header = NP.PPMHeader NP.P3 0 0 
                in NP.PPM header imgData
    where
     toList :: Store Coord RGB -> [NP.PpmPixelRGB8]
     toList st@(Store (Coord row col) imgf) =
                 if current st == (RGB 0 0 0)
                 then 
                     []
                 else
                     let (RGB r g b) = current st
                         pixel =  (NP.PpmPixelRGB8 r g b)
                     in pixel:(toList (Store (Coord row (col+1))imgf))




-- IO 
main = do
        result <- reader
        (image:_) <- handlePPM result
        getStatus result
        writeFile "WriteTo.ppm" $ serialize image
        let NP.PPM header imgData = image
        let Store (Coord c _) _ = ppmToStore image
        print c 
        return (header, imgData)
        -- writeFile "WriteTo.ppm" $ serilize image
    

reader :: IO NP.PpmParseResult
reader = do
            file <- BS.readFile "Sample.ppm"
            return $ NP.parsePPM file


serialize :: NP.PPM -> String
serialize (NP.PPM (NP.PPMHeader t w h) img)  =
                let intList = NP.pixelDataToIntList img 
                    headStr = foldl1 (\acc x-> x ++ "\n" ++ acc) 
                                [show (maximum intList), show h, show w, "P3"] ++ "\n"
                in headStr ++ (unlines $ map show intList)


handlePPM :: NP.PpmParseResult -> IO [NP.PPM]
handlePPM (Right (images, rest)) = return images
handlePPM (Left err) = do
  putStrLn "Failed to parse PPM image:"
  putStrLn err
  exitFailure


getStatus :: NP.PpmParseResult -> IO ()
getStatus (Right file) = let result = case file of
                                ([], _)           -> ""
                                (images, Nothing) -> concat $ map show images
                                (_, Just _)       -> show file
                        in print result
getStatus (Left err) = do
  putStrLn "Failed to get status PPM image:"
  putStrLn err
  exitFailure
