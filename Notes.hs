{-# LANGUAGE OverloadedStrings #-}

module Notes
    ( recommendNote
    , recommendNotes
    ) where

import Data.List
import Data.Ord
import Data.Text (Text)

noteTypes :: [Text]
noteTypes = [ "AA", "A", "B", "C", "D", "E", "HR" ]

recommendNotes :: (Ord a, Fractional a, Integral b) => [Text] -> [a] -> b -> [Text]
recommendNotes _ _ 0 = []
recommendNotes notes targetDist 1 = [recommendNote notes targetDist]
recommendNotes notes targetDist n = nextNote : recommendNotes (nextNote:notes) targetDist (n-1)
    where
        nextNote = recommendNote notes targetDist

recommendNote :: (Num a, Ord a, Fractional a) => [Text] -> [a] -> Text
recommendNote notes targetDist
    | length targetDist /= 7 = error "Invalid target distribution"
    | sum targetDist /= 1 = error "Invalid target distribution"
    | any (<0) targetDist = error "Invalid target distribution"
    | all (==0) distributionDiff = snd (maximumBy (comparing fst) x)
    | otherwise = snd (maximumBy (comparing fst) y)
    where
        x = zip targetDist noteTypes
        y = zip distributionDiff noteTypes
        distributionDiff = subtractLists targetDist currentDist
        currentDist = distribution notes
        subtractLists = zipWith (-)

distribution :: (Fractional a) => [Text] -> [a]
distribution = percentize . noteCount

percentize :: (Real a, Fractional b) => [a] -> [b]
percentize xs = map (/ sum fracList) fracList
    where
        fracList = map realToFrac xs

noteCount :: (Num a) => [Text] -> [a]
noteCount notes = [ count noteType notes | noteType <- noteTypes ]

count :: (Eq a, Num b) => a -> [a] -> b
count x = genericLength . filter (x==)
