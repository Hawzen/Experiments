module Main where

import Data.Char
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
        let file@(images, rest) = handlePPM result
        getStatus file
        -- return $ map createStore images
    

reader :: IO NP.PpmParseResult
reader = do
            file <- BS.readFile "Sample.ppm"
            return $ NP.parsePPM file

-- handler ::
handlePPM :: NP.PpmParseResult -> ([NP.PPM], Maybe BS.ByteString)
handlePPM (Left err) = error err
handlePPM (Right result) = result


getStatus :: ([NP.PPM], Maybe BS.ByteString) -> IO ()
getStatus file = case file of
                        ([], _)           -> error "Empty images"
                        (images, Nothing) -> mapM_ print images
                        (_, Just _)       -> do
                                                print ("Image partially parsed\n")
                                                print file


                    