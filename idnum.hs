module IdNum (prcIdCoeffs, prcIdVerifys, calcVerify) where

import Data.Char (digitToInt)

type Verify = Char

prcIdCoeffs :: [Int]
prcIdVerifys :: [Verify]

calcVerify :: String -> Verify

--
prcIdCoeffs = [7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2]
prcIdVerifys = "10X98765432"
--

calcVerify idn = prcIdVerifys !! index
  where
    index = checksum `mod` (length prcIdVerifys)
    checksum = sum multiply
    multiply = zipWith (\d i -> (readChar d) * i) idn prcIdCoeffs
    readChar = digitToInt
