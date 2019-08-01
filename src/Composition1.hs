module Composition1
    (comp1) where

import Euterpea
import Grammar
import Euclid
import Utility
import EuterpeaAdditions
import Data.Tuple (swap)
import Control.Monad (join)
import Control.Arrow ((***))

-- Song 1

strToMusic :: Pitch -> Dur -> (PitchClass, [Int]) -> EuclideanRhythm -> String -> Music Pitch
strToMusic _ _ _ _ [] = rest 0
strToMusic p d m@(root, is) r@(o,ds) (s:ss) =
    let
        mode = euclidToMode is
        mapTuple = join (***)
        spanLong c x = swap $ mapTuple reverse $ span (/= c) $ reverse x
        durForPulses 0 = 0
        durForPulses n = d * (toRational $ ds !! ((o + n) `mod` length ds)) + durForPulses (n - 1)
        newRhythm n = ((o + n) `mod` length ds, ds)
        c scaleDegrees len =
            let chordNote i = note (durForPulses len) (transInScale root mode i p)
            in map chordNote scaleDegrees
        breakChord notes =
            let divLength = toRational (length notes)
            in line $ map (scaleDurations divLength) notes
        makeChord notes = chord [head notes, instrument AcousticGrandPiano $ chord notes]
    in
        case s of
            'X' -> strToMusic p d m r ss
            'Y' -> strToMusic p d m r ss
            'Z' -> strToMusic p d m r ss
            'H' -> strToMusic p d m r ss
            'N' -> note (durForPulses 3) p :+: strToMusic p d m (newRhythm 3) ss
            'n' -> note (durForPulses 1) p :+: strToMusic p d m (newRhythm 1) ss
            'C' ->
                let
                    nearPitch ap1 ap2
                        | abs (ap2 - ap1) <= 6 = ap1
                        | otherwise = nearPitch (ap1 + 12) ap2
                    pRoot = pitch $ nearPitch (pcToInt root) (absPitch p)
                    p2nd = pitch $ nearPitch (pcToInt $ fst $ transInScale root mode (3) pRoot) (absPitch p)
                in line
                    [ note (durForPulses 2) p2nd
                    , note (durForPulses 1) pRoot
                    , note (durForPulses 2) pRoot
                    , note (durForPulses 3) pRoot
                    , strToMusic p d m (newRhythm 1) ss
                    ]
            '3' -> makeChord (c [0,2,4] 2) :+: strToMusic p d m (newRhythm 2) ss
            '4' -> makeChord (c [-2,0,3] 2) :+: strToMusic p d m (newRhythm 2) ss
            '+' ->
                let newP = transInScale root mode 1 p
                in strToMusic newP d m r ss
            '-' ->
                let newP = transInScale root mode (-1) p
                in strToMusic newP d m r ss
            '[' ->
                let (hd, tl) = spanLong ']' ss
                in strToMusic p d m r hd :+: strToMusic p d m r tl
            ']' -> strToMusic p d m r ss
            '{' -> 
                let
                    (hd, tl) = spanLong '}' ss
                    (newRoot, _) = pitch $ ((pcToInt root) `mod` 12) + 7
                    newMode = (newRoot, rotateL 2 is)
                in
                    strToMusic p d newMode r hd :+: strToMusic p d m r tl
            '}' -> strToMusic p d m r ss

g1 = DetGrammar 'X'
    [ ('X', "N+[[X]-X]+n+X-3[n+{n[+X]-4}]-X-nC")
    , ('N', "N")
    , ('n', "nn")
    , ('C', "C")
    , ('3', "3")
    , ('4', "4")
    , ('+', "+")
    , ('-', "-")
    , ('[', "[")
    , (']', "]")
    , ('{', "{")
    , ('}', "}")
    ]

comp1 :: Music Pitch
comp1 = instrument Vibraphone $ (strToMusic pitch qn mode rhythm $ detGenerate g1 3) :+: cadence
    where
        pitch = (C, 3)
        mode = (C, euclid 7 12)
        rhythm = (0, superEuclid [7, 3, 5, 2] 32)
        ln = 2 * wn
        cadence = note ln (G, 2) :+: note hn (C, 3) :+: note ln (C, 3) :+: note ln (C, 3)

g2 = DetGrammar 'X'
    [ ('X', "H+nN-NX+N{N{X}}-nC")
    , ('H', "n+n+n+n+n+n-N-N-nN[X]--")
    , ('N', "N")
    , ('n', "n")
    , ('C', "C")
    , ('3', "3")
    , ('4', "4")
    , ('+', "+")
    , ('-', "-")
    , ('[', "[")
    , (']', "]")
    , ('{', "{")
    , ('}', "}")
    ]

comp2 :: Music Pitch
comp2 = instrument Vibraphone $ (strToMusic pitch qn mode rhythm $ detGenerate g2 4) :+: cadence
    where
        pitch = (G, 2)
        mode = (G, euclid 7 12)
        rhythm = (0, superEuclid [13, 11] 48)
        ln = 2 * wn
        cadence = note ln (G, 2) :+: note hn (C, 3) :+: note ln (C, 3) :+: note ln (C, 3) :+: note ln (C, 3)

g3 = DetGrammar 'X'
    [ ('X', "YH+n[-n]+{[-X]+X}Z-N-C")
    , ('Y', "Nn---Z")
    , ('Z', "Nn+++Y")
    , ('H', "n+n+n+n+n+n+N[{X}-Y]N--N--nN-n-")
    , ('N', "N")
    , ('n', "n")
    , ('C', "C")
    , ('3', "3")
    , ('4', "4")
    , ('+', "+")
    , ('-', "-")
    , ('[', "[")
    , (']', "]")
    , ('{', "{")
    , ('}', "}")
    ]

comp3 :: Music Pitch
comp3 = instrument Vibraphone $ (strToMusic pitch qn mode rhythm $ detGenerate g3 5) :+: cadence
    where
        pitch = (F, 2)
        mode = (F, euclid 7 12)
        rhythm = (0, superEuclid [5,7] 32)
        ln = 2 * wn
        cadence = note ln (B, 2) :+: note hn (F, 3) :+: note ln (F, 3) :+: note ln (F, 3) :+: note ln (F, 3)
