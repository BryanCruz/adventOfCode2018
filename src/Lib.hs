module Lib
    ( chronalCalibration
    ) where

-- Day 1

chronalCalibration :: IO ()
chronalCalibration = do
    content <- getContents
    let numbers = map getInt (lines content)
        result  = sum numbers
    putStrLn $ show result

-- Util
getInt :: [Char] -> Integer
getInt ('-':xs) = -(getInt xs)
getInt ('+':xs) = getInt xs
getInt       xs = read xs :: Integer

