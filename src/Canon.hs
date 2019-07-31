module Canon
    ()
where

import Euterpea
import Data.Map.Strict (Map, fromListWith, toList)
import Data.List (tails)
import Data.Maybe (fromMaybe, catMaybes)
import Control.Applicative

data Motion = Oblique | Contrary | Similar | Parallel

motion :: Int -> Int -> Motion
motion i1 i2
    | i1 == 0 || i2 == 0 = Oblique
    | i1 == i2           = Parallel
    | i1 * i2 > 0        = Similar
    | otherwise          = Contrary

repeatIntoLine :: Int -> Music Pitch -> Music Pitch
repeatIntoLine n m = line $ concat $ replicate n $ lineToList m

canonIntervals :: [Int]
canonIntervals = let is = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12] in map (* (-1)) (reverse is) ++ [0] ++ is

canonOnsetDistances :: [Rational]
canonOnsetDistances = [(1 / 2), (1 / 4), (3 / 4), (1 / 8), (3 / 8), (5 / 8), (7 / 8)]

getDurAndAbsPitch :: Music Pitch -> (Dur, Maybe AbsPitch)
getDurAndAbsPitch (Prim (Note d p)) = (d, Just (absPitch p))
getDurAndAbsPitch (Prim (Rest d  )) = (d, Nothing)
getDurAndAbsPitch _                 = error "getDurAndAbsPitch: can only get duration and pitch from a primitive"

sortAndGroup :: Ord k => [(k, a)] -> Map k [a]
sortAndGroup assocs = fromListWith (++) [ (k, [v]) | (k, v) <- assocs ]

-- TODO: check that denominator of dur m is 1
-- TODO: check distances cleverly based on dur m
findCanons :: Int -> Music Pitch -> [(AbsPitch, Dur)]
findCanons numVoices m = filter (uncurry (check m)) combinations
  where
    combinations = [ (i, d) | i <- canonIntervals, d <- canonOnsetDistances ]
    check m i d = (== 0) . length $ findIllegalNotes $ canonize numVoices 1 i d m

findCanonErrors :: Int -> Music Pitch -> [(AbsPitch, Dur, ((Int, Pitch), (Int, Pitch)))]
findCanonErrors numVoices m = catMaybes $ map (uncurry (findErrors m)) combinations
  where
    combinations = [ (i, d) | i <- canonIntervals, d <- canonOnsetDistances ]

    getIntervalAndPitch :: [((Int, Int), (Int, Int))] -> Maybe ((Int, Pitch), (Int, Pitch))
    getIntervalAndPitch (((i1, p1), (i2, p2)) : _) = Just ((i1, pitch p1), (i2, pitch p2))
    getIntervalAndPitch _                          = Nothing

    findErrors m i d =
        let firstFailingNote = getIntervalAndPitch $ findIllegalNotes $ canonize numVoices 1 i d m
        in fmap (\err -> (i, d, err)) firstFailingNote

canonize :: Int -> Int -> Int -> Rational -> Music Pitch -> [Music Pitch]
canonize numVoices times i d m = map (createVoice) [0 .. (numVoices - 1)]
    where createVoice n = rest (toRational n * d * dur m) :+: (repeatIntoLine times $ shiftPitches (n * i) m)

findIllegalNotes :: [Music Pitch] -> [((Int, Int), (Int, Int))]
findIllegalNotes ms = concat $ map findIllegalNotesInGroup $ groupedNotes ms
  where
    accumulator
        :: [(Dur, AbsPitch, Dur, Maybe AbsPitch)] -> (Dur, Maybe AbsPitch) -> [(Dur, AbsPitch, Dur, Maybe AbsPitch)]
    accumulator ns (dur, p) = case ns of
        [] -> [(0, 0, dur, p)]
        prevNs@((prevPos, _, prevDur, prevP) : _) ->
            let
                newPos   = prevPos + prevDur
                interval = case (prevP, p) of
                    (Just prevPitch, Just pitch) -> pitch - prevPitch
                    _                            -> 0
                newPitch = p <|> prevP
            in ((newPos, interval, dur, newPitch) : prevNs)

    shortestDur = minimum $ map dur ms
    rationalMod x y
        | x < y     = x
        | otherwise = rationalMod (x - y) y
    prepareNotes ns =
        map (\(pos, i, _, p) -> (pos `rationalMod` shortestDur, (i, p))) $ removeRests $ foldl accumulator [] ns
    fromMusic = ((map getDurAndAbsPitch) . lineToList)
    groupedNotes ms = toList $ sortAndGroup $ concat $ map prepareNotes $ map fromMusic ms

checkNotes :: (Int, Int) -> (Int, Int) -> Bool
checkNotes (i1, p1) (i2, p2) =
    let pInterval = abs (p1 - p2)
    in
        case motion i1 i2 of
            Oblique  -> True
            Contrary -> elem pInterval [3, 4, 5, 6, 7, 8, 9]
            Parallel -> elem pInterval [3, 4, 8, 9]
            Similar  -> elem pInterval [3, 4, 5, 8, 9]

findIllegalNotesInGroup :: (Dur, [(AbsPitch, AbsPitch)]) -> [((Int, Int), (Int, Int))]
findIllegalNotesInGroup (_, ns) =
    let pairs = [ (x, y) | (x : ys) <- tails ns, y <- ys ] in filter (not . (uncurry checkNotes)) pairs

removeRests :: [(Dur, AbsPitch, Dur, Maybe AbsPitch)] -> [(Dur, AbsPitch, Dur, AbsPitch)]
removeRests []                            = []
removeRests ((_  , _, _  , Nothing) : xs) = removeRests xs
removeRests ((pos, i, dur, Just p ) : xs) = ((pos, i, dur, p) : removeRests xs)
