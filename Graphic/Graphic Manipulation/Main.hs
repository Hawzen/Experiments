module Main where
import Data.Char
import qualified Data.ByteString as BS
import qualified Graphics.Netpbm as NP

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


main = do
		file <= BS.readFile "Sample.ppm"
		NP.parsePPM file
























 {-
[(1,'A'),(2,'B'),(3,'C'),(4,'D'),(5,'E'),(6,'F'),(7,'G'),(8,'H'),
 (9,'I'),(10,'J'),(11,'K'),(12,'L'),(13,'M'),(14,'N'),(15,'O'),
 (16,'P'),(17,'Q'),(18,'R'),(19,'S'),(20,'T'),(21,'U'),(22,'V'),
 (23,'W'),(24,'X'),(25,'Y'),(26,'Z')]
  -}

-- toChar :: Int -> Char
-- toChar i = ['A'..'Z'] !! (i `mod` 26)

-- charStore = Store 10 toChar -- -> 'K'


-- main :: IO ()
-- main = do
--         let a = peek (pos charStore) charStore
--         let b = current (fmap toLower charStore)
--         print [a, b]
