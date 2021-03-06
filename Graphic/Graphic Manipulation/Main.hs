module Main where

import Data.Char
import Data.Binary
import Debug.Trace
import Data.List.Split
import System.Exit (exitFailure)
import Control.Applicative (liftA2)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString as BS
import qualified Graphics.Netpbm as NP
import qualified Foreign.Storable as ST
import qualified Data.Vector.Storable as V


-- Data Types


data Store s a = Store s (s -> a)
data Coord = Coord Int Int
data RGB = RGB Word8 Word8 Word8
type Kernel = (Store Coord RGB -> RGB)


-- Instances


instance Eq Coord where
    (Coord a b) == (Coord x y) = a == x && b == y


instance Show Coord where
    show (Coord x y) = show x ++ " " ++ show y


instance Num RGB where
    (RGB r g b) * (RGB r2 g2 b2) = (RGB (r*r2) (g*g2) (b*b2))
    (RGB r g b) + (RGB r2 g2 b2) = (RGB (r+r2) (g+g2) (b+b2))
    abs rgb = rgb
    signum rgb = 1
    fromInteger i =  let n = fromIntegral i
                     in  (RGB n n n)
    negate (RGB r g b) = (RGB (-r) (-g) (-b))

-- clamp :: (Integer -> Integer -> Integer) -> Word8 -> Word8 -> Word8
-- clamp f x1 x2 =
--     let i1 = toInteger x1
--         i2 = toInteger x2
--     in fromIntegral $ min 255 (f i1 i2) 



instance Show RGB where
    show (RGB r g b) = unwords $ map show [r, g, b]


instance Functor (Store s) where
    fmap f (Store s fs) = Store s (f . fs)


-- Store Functions


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


-- Image Stuff


applyFilter :: (Store Coord RGB -> Store Coord RGB) -> NP.PPM -> NP.PPM
applyFilter filt ppm@(NP.PPM header@(NP.PPMHeader t w h) img) = 
        let 
            endCoord = (Coord w h)
            store = ppmToStore ppm
            filtStore = filt store
        in storeToppm filtStore header
    where
            -- PPM to Store
            ppmToStore :: NP.PPM -> Store Coord RGB
            ppmToStore (NP.PPM (NP.PPMHeader _ w h) img) = 
                    let pixels = chunksOf w $ toPixel $ NP.pixelDataToIntList img
                        imgf (Coord row col) = 
                            if  h > row && w > col && row >= 0 && col >= 0
                                then pixels !! row !! col
                                else (RGB 0 0 0)
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


            -- Store to PPM 
            storeToppm :: Store Coord RGB -> NP.PPMHeader -> NP.PPM
            storeToppm st header@(NP.PPMHeader t w h) = 
                    let 
                        -- rgb8List = toList (seek (Coord 0 0) st) (Coord w h)
                        allcoords = liftA2 (Coord) [0..h-1] [0..w-1]
                        rgb8List = foldr (\s acc -> 
                                            let (RGB r g b) = peek s st
                                            in (NP.PpmPixelRGB8 r g b):acc
                                         ) [] allcoords
                        imgData = NP.PpmPixelDataRGB8 $ V.fromList rgb8List
                    in NP.PPM header imgData


-- Sharpen [[0,-1,0], [-1, 5, -1], [0,-1,0]]
sharpen :: Store Coord RGB -> RGB
sharpen st =
        (-1) * (mv 0    (-1) st)  +

        (-1) * (mv (-1) 0    st)  +
        5    * (mv 0    0    st)  +
        (-1) * (mv 1    0    st)  +
     
        (-1) * (mv 0    1    st)
    where
        mv i j st = 
            let (Coord x y) = pos st
            in peek (Coord (x+i) (y+j)) st        


convolve :: Kernel -> Store Coord RGB -> Store Coord RGB
convolve ker st@(Store coord img) = 
        let newImg c = ker $ seek c st
        in (Store coord newImg)


compose :: Kernel -> Kernel -> Store Coord RGB -> RGB
compose ker1 ker2 st@(Store coord img) = 
        let newImg c = ker1 $ seek c st
            st' = (Store coord newImg)
        in ker2 $ seek coord st'


-- IO Stuff


main = 
        do
            result <- reader
            (image:_) <- handlePPM result
            let filteredImg = applyFilter (convolve sharpen) image
            printStatus result
            writeFile "WriteTo.ppm" $ serialize filteredImg
            return ()
    

reader :: IO NP.PpmParseResult
reader = do
            file <- BS.readFile "Sample.ppm"
            return $ NP.parsePPM file


serialize :: NP.PPM -> String
serialize (NP.PPM (NP.PPMHeader t w h) img)  =
                let intList = NP.pixelDataToIntList img 
                    headStr = unlines ["P3", show h, show w, show (maximum intList)]
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
