module Main where
import System.IO

intro = "281 Project | Fall 2020 | Mohand Alrasheed"
main :: IO ()
main = do
    putStrLn intro
    eqs <- getEquations
    let ms = map m eqs
        as = map a eqs
        -- bs = map b eqs
        (bigM, bigMs) = calcMs ms
    if coprimes ms 
        then let summation = sum $ map (solutionTerm bigM) eqs
                 ans = summation `mod` bigM
                 bs = map (\(a, m) -> a * ans `rem` m) $ zip as ms
                 checks = map (\(a, m, b) -> 
                    "\t" ++ (show ans) ++ " * " ++ (show a) ++ " mod " ++ (show m) ++ " = " ++ (show b) ++ "\n"
                    ) $ zip3 as ms bs
             in do
                    putStrLn $ "X: " ++ (show ans)
                    putStrLn "Checks:"
                    putStrLn $ mconcat checks
                    putStrLn "Again? (y\\n): "
                    again <- getLine
                    if ('y' `elem` again) then main else return ()
        else putStrLn "Error: No solution (modulos are not coprimes)"


type Modulo = Integer
data Equation = Equation { a :: Integer
                         , x :: Integer
                         , b :: Integer
                         , m :: Modulo
                         } deriving (Eq, Show)

getEquation :: IO Equation
getEquation = do 
            putStr "Enter an Equation (a b m): "
            hFlush stdout
            nums <- getLine
            let (a:b:m:[]) = map read $ words nums
            return (check (Equation a (-1) b m))

check :: Equation -> Equation
check eq@(Equation a _ _ m) 
    | coprime a m = eq
    | otherwise = error $ "Coprime Error: " ++ (show a) ++ " And " ++ (show m) ++ " are not coprime"

getEquations :: IO [Equation]
getEquations = do 
            putStr "Enter the number of equations: "
            hFlush stdout
            n <- getInt :: IO Int
            sequence . take n $ repeat getEquation
        where
            getInt = fmap read getLine

coprime :: Integer -> Integer -> Bool
coprime a b = gcd' a b == 1

-- Pairwise co-primality
coprimes :: [Modulo] -> Bool
coprimes [] = True
coprimes [x] = True
coprimes (x:xs) = and (map (coprime x) xs)
                  && coprimes xs


gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = let (q, r) = a `divMod` b
           in gcd' b r

calcMs :: [Modulo] -> (Modulo, [Modulo])
calcMs ms  = let bigM = product ms
                 bigMs = map (div bigM) ms
             in (bigM, bigMs)

inverse :: Integer -> Modulo -> Integer
inverse a m = let condition x = a * x `mod` m == 1
                  list = filter condition [1..m]
              in if null list then error "No inverse [EDIT ME LATER]"
                              else head list 

solutionTerm :: Modulo -> Equation -> Integer
solutionTerm bigM (Equation a x b m) = 
        let a' = inverse a m
            bigMi = div bigM m
            y = inverse bigMi m
        in a' * b * bigMi * y


{-
1- Time complexity
2- Data structure used (I used a list of Equations)
3- "Where all the variables are integer"
    * Is 0 allowed?
    * Inverse of 1 mod 1
    * Negative numbers allowed?
4- Build is for windows 64 bit
-}