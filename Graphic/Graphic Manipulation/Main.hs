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


-- Image functions

data Coord = Coord Int Int

-- createStore :: [NP.PPM] -> (Store a s)
-- createStore img = Store (Coord 0 0) (ST.peek)

-- | [R, G, B, R, G, ...] -> [[R,G,B], [R,G,B], ...]
toPixel :: [a] -> [[a]]
toPixel (x:px) = 
            foldr (\(i,a) (h:acc) -> if i `mod` 3 == 0
                            then    [a]:(h:acc)
                            else    (a:h):acc
                            )
                [[x]] $ zip [1..] px

-- IO 
main = do
        result <- reader
        (image:_) <- handlePPM result
        getStatus result
        writeFile "WriteTo.ppm" $ serialize image
        let NP.PPM header imgData = image
        return (header, imgData)
        -- writeFile "WriteTo.ppm" $ serilize image
    

reader :: IO NP.PpmParseResult
reader = do
            file <- BS.readFile "Sample.ppm"
            return $ NP.parsePPM file


-- PPM P6 image (256,256)
serialize :: NP.PPM -> String
serialize (NP.PPM (NP.PPMHeader t w h) img)  =
                let intList = NP.pixelDataToIntList img 
                    headStr = foldl1 (\acc x-> x ++ "\n" ++ acc) 
                                [show (maximum intList), show h, show w, "P3"] ++ "\n"
                    pixels = toPixel $ intList-- [[R,G,B], [R,G,B], ...]
                    formatted = -- "R G B\tR G B\t....\nR G B\t..."
                     foldr (\(i,rgb) acc -> 
                        (concatMap (\x -> show x ++ " ") rgb++)
                            $ if i `mod` 10 == 0
                                        then '\n':acc
                                        else ' ':acc) 
                            "" $ zip [1..] pixels
                in headStr ++ formatted


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
