module Main where

import Data.Char
import Data.Binary
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



-- IO 
main = do
        result <- reader
        (image:_) <- handlePPM result
        print image
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
                let headStr = foldl1 (\acc x-> x ++ "\n" ++ acc) 
                                ["65535", show h, show w, show t] ++ "\n"
                in headStr ++ (show $ encode $ NP.pixelDataToIntList img)


handlePPM :: NP.PpmParseResult -> IO [NP.PPM]
handlePPM (Right (images, rest)) = return images


getStatus :: ([NP.PPM], Maybe BS.ByteString) -> String
getStatus file = case file of
                        ([], _)           -> ""
                        (images, Nothing) -> concat $ map show images
                        (_, Just _)       -> show file