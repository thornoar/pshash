module Info where

import Algorithm

-- ┌───────────────────────────┐
-- │ COUNTING/PRINTING NUMBERS │
-- └───────────────────────────┘

numberOfHashes :: [(Integer, Integer)] -> Integer
numberOfHashes amts = product (zipWith cnk fsts snds) * factorial (sum snds)
  where
    fsts = map fst amts
    snds = map snd amts

numberOfHashes' :: [([Char], Integer)] -> Integer
numberOfHashes' = numberOfHashes . map dropElementInfo

numberOfChoiceKeys :: [(Integer, Integer)] -> Integer
numberOfChoiceKeys = chooseAndMergeSpread

numberOfChoiceKeys' :: [([a], Integer)] -> Integer
numberOfChoiceKeys' = chooseAndMergeSpread'

numberOfShuffleKeys :: [Integer] -> Integer
numberOfShuffleKeys = factorial . sum

numberOfShuffleKeys' :: [([a], Integer)] -> Integer
numberOfShuffleKeys' = numberOfShuffleKeys . map snd

numberOfRepetitions :: [Integer] -> Integer
numberOfRepetitions = numberOfShuffleKeys

numberOfPublicKeys :: [(Integer, Integer)] -> Integer
numberOfPublicKeys = numberOfChoiceKeys

numberOfPublicKeys' :: [([a], Integer)] -> Integer
numberOfPublicKeys' = numberOfChoiceKeys'

maxLengthOfPublicKey :: [(Integer, Integer)] -> Integer
maxLengthOfPublicKey amts = getBiggestPower 0 $ (length' . show) (numberOfPublicKeys amts)
  where
    get128PowerLength :: Integer -> Integer
    get128PowerLength p = 2 * p + 3 * pDiv + npMod where
      (pDiv, pMod) = divMod p 28
      npMod
        | pMod < 10 = 1
        | pMod < 19 = 2
        | otherwise = 3
    getBiggestPower :: Integer -> Integer -> Integer
    getBiggestPower guess bound
      | get128PowerLength (guess + 1) < bound = getBiggestPower (guess + 1) bound
      | otherwise = guess + 1

timeToCheckPower :: Integer
timeToCheckPower = -10

timeToCheckPicos :: Integer
timeToCheckPicos = 10 ^ (timeToCheckPower + 12)

psInYear :: Integer
psInYear = 31557600000000000000

ageOfUniverseYears :: Integer
ageOfUniverseYears = 13787000000

timeToCrack :: Integer -> Integer
timeToCrack num = div (timeToCheckPicos * num) (2 * psInYear)
  -- where
  --   inYears = div (timeToCheckPicos * num) (2 * psInYear)
  --   inAgesOfUniverse = div inYears ageOfUniverseYears

getPowerOf :: Integer -> Integer -> Integer
getPowerOf b n
  | n < b = 0
  | otherwise = 1 + getPowerOf b (div n b)

printTimes :: String -> Integer -> String
printTimes pr inY = pr ++ " : "
  ++ if (inY == 0) then "< 1 year"
     else if (inY < 1000) then show inY ++ " years"
     else show inY ++ " > 10^" ++ show (getPowerOf 10 inY) ++ " years"
  -- ++ show inY ++ " > 10^" ++ show (getPowerOf 10 inY) 
  -- ++ " years"
  -- ++ (if inAoU > 0
  --    then "\n| or "
  --         ++ show inAoU ++ " > 10^" ++ show (getPowerOf 10 inAoU)
  --         ++ " ages of the Universe"
  --    else "")

printBits :: Integer -> String
printBits num = "2^" ++ show (getPowerOf 2 num)
