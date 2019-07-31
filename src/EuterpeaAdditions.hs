module EuterpeaAdditions
    (transInScale,
    appoggiatura,
    mordent,
    phaseIt) where

import Euterpea
import Utility
import Data.Maybe (fromMaybe)
import Data.List (findIndex)

{-| @transInScale@ takes a scale (pitch class and list of intervals), an
    interval and a pitch, and returns the pitch moved by the interval within the
    given scale.
-}
transInScale :: PitchClass -> [Int] -> Int -> Pitch -> Pitch
transInScale pc mode i = f
    where
        buildOctave oct = map (\n -> n + oct * 12) mode
        notesInScale = map (+ (pcToInt pc)) $ concatMap buildOctave [0..8]
        fAbs p = findIndex (>= p) notesInScale >>= \idx -> nth (idx + i) notesInScale
        f p = fromMaybe p $ fmap pitch $ fAbs $ absPitch p

appoggiatura :: Int -> Rational -> Music Pitch -> Music Pitch
appoggiatura n r (Prim (Note d p)) =
    note (r * d) (trans n p) :+: note ((1 - r) * d) p
appoggiatura _ _ _ =
    error "appoggiatura: can only add an appoggiatura to a note"

mordent :: Int -> Dur -> Music Pitch -> Music Pitch
mordent i mDur (Prim (Note d p)) =
    note mDur p :+: note mDur (trans i p) :+: note (d - 2 * mDur) p
mordent _ _ _ =
    error "mordent: can only add a mordent to a note"

phaseIt :: Rational -> Music a -> Music a
phaseIt factor m = m :=: tempo factor m

{-| @rep@ takes two Music-modifying functions and a number and repeatedly
    applies the functions to the music in a very confusing manner ... Perhaps it
    can be useful.
-}
rep :: (Music a -> Music a) -> (Music a -> Music a) -> Int -> Music a -> Music a
rep f g 0 m = rest 0
rep f g n m = m :=: g (rep f g (n - 1) (f m))
