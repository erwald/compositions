module Composition0
    ()
where

import Euterpea
import EuterpeaAdditions
import Euclid
import Utility
import Data.Maybe (fromMaybe)

-- Euclid -> Music

euclidToMusic :: Dur -> Pitch -> EuclideanRhythm -> Music Pitch
euclidToMusic dur p (o, ds) | o > 0 = initialRest :+: euclidToMusic dur p (0, ds)
    where initialRest = Prim (Rest ((toRational o) * dur))
euclidToMusic dur p (_, ds) = line $ map toNote ds where toNote d = note ((toRational d) * dur) p

-- Variations

data Variation = Appoggiatura Int | Mord Int | Substitute Pitch

appog :: Int -> Variation
appog = Appoggiatura

mord :: Int -> Variation
mord = Mord

sub :: Pitch -> Variation
sub = Substitute

applyVariation :: Variation -> Music Pitch -> Music Pitch
applyVariation (Appoggiatura i) = appoggiatura i (1 / 32)
applyVariation (Mord         i) = mordent i tn
applyVariation (Substitute   p) = mMap (return p)

euclidToMusicVaried :: Dur -> Pitch -> [(Int, Variation)] -> EuclideanRhythm -> Music Pitch
euclidToMusicVaried dur p vars (o, ds) | o > 0 = initialRest :+: euclidToMusicVaried dur p vars (0, ds)
    where initialRest = Prim (Rest ((toRational o) * dur))
euclidToMusicVaried dur p vars (_, ds) = line $ map (uncurry toNote) dsWithVars
  where
    toNote d []           = note ((toRational d) * dur) p
    toNote d (var : vars) = applyVariation var $ toNote d vars
    packVars idx = map snd $ filter ((== idx) . (`mod` (length ds)) . fst) vars
    dsWithVars = zip ds $ map packVars $ [0 .. (length ds - 1)]

-- Composition

generateRhythms2 :: Int -> Int -> [Int] -> Int -> [Int] -> [EuclideanRhythm]
generateRhythms2 totalDur o1 k1 o2 k2 =
    monoizeDistancesL [(o1, superEuclid k1 totalDur), (o2, superEuclid k2 totalDur)]

generateRhythms3 :: Int -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> [EuclideanRhythm]
generateRhythms3 totalDur o1 k1 o2 k2 o3 k3 =
    monoizeDistancesL [(o1, superEuclid k1 totalDur), (o2, superEuclid k2 totalDur), (o3, superEuclid k3 totalDur)]

genCycle :: [Pitch] -> [[(Int, Variation)]] -> [EuclideanRhythm] -> Music Pitch
genCycle ps vs rs = chord $ map (uncurry3 $ euclidToMusicVaried qn) $ zip3 ps vs rs

dance1 :: Int -> Music Pitch
dance1 section = line $ map (uncurry3 genCycle) $ drop
    section
                -- Introduction
    [ ( [(Af, 2), (C, 4)]
      , [[(-2, appog 2)], [(5, mord 1), (-8, sub (Bf, 3)), (-5, sub (Bf, 3)), (-3, mord 1)]]
      , generateRhythms2 (4 * 24) 0 [4, 5, 7, 10] 0 [2, 3, 4, 6, 7]
      )
                -- Section 1
    , ( [(F, 1), (Af, 2), (C, 4)] -- i/f
      , [[], [(3, mord 2)], [(0, appog 1), (5, mord 1)]]
      , generateRhythms3 (3 * 24) 0 [9, 9, 9] 0 [15, 15, 15] 1 [11, 7, 11]
      )
    , ( [(F, 1), (Af, 2), (Df, 4)] -- VI
      , [[(-2, sub (Ef, 1))], [(3, mord 2), (-1, sub (Bf, 2))], [(2, appog 2), (3, mord 2)]]
      , generateRhythms3 (3 * 24) 0 [9, 6, 9] 0 [9, 13, 9, 13] 1 [7, 9, 7]
      )
    , ( [(F, 1), (Bf, 2), (Df, 4)] -- iv
      , [[(-3, sub (Ef, 1)), (-1, mord 2)], [(3, mord (-2))], [(4, mord 2)]]
      , generateRhythms3 (3 * 24) 0 [9, 6, 9] 0 [11, 13, 15] 0 [1, 11, 13, 11]
      )
    , ( [(G, 1), (Bf, 2), (Ef, 4)] -- VII = III/c
      , [[(-3, sub (F, 1)), (-1, mord 1)], [(3, mord (-2))], [(0, appog (-2)), (2, mord 2), (5, mord 2)]]
      , generateRhythms3 (3 * 24) 0 [9, 8, 9] 0 [10, 12, 15] 0 [3, 7, 9, 11]
      )
    , ( [(G, 1), (B, 2), (D, 4)] -- V
      , [[(-3, sub (F, 1))], [(1, appog 2), (-1, mord 2)], [(5, mord 1)]]
      , generateRhythms3 (2 * 24) 0 [3, 9] 0 [13, 13] 1 [11, 13, 9]
      )
    , ( [(G, 1), (Bf, 2), (Ef, 4)] -- III
      , [[(0, mord 1), (-2, sub (F, 1))], [(3, sub (Af, 2)), (-3, sub (Af, 2)), (-1, sub (B, 2))], [(5, mord 2)]]
      , generateRhythms3 (3 * 24) 0 [9, 6, 3] 1 [15, 11, 7] 2 [11, 9, 1]
      )
            -- Section 2
    , ( [(G, 1), (C, 3), (Ef, 4)] -- i
      , [ [(0, mord 1), (4, mord 1), (-2, sub (F, 1))]
        , [(2, appog 2), (-10, mord 2)]
        , [(-8, sub (D, 4)), (-3, sub (D, 4)), (-5, mord 2)]
        ]
      , generateRhythms3 (3 * 24) 2 [9, 9, 9] 1 [10, 6, 8, 10] 0 [13, 13, 13]
      )
    , ( [(Af, 1), (C, 3), (Ef, 4)] -- VI
      , [ [(0, mord 1), (4, mord 1), (-2, sub (Bf, 1))]
        , [(0, sub (F, 4)), (10, sub (D, 3))]
        , [(3, mord 2), (-3, mord 2)]
        ]
      , generateRhythms3 (3 * 24) 0 [9, 9, 9] 1 [9, 15, 9] 0 [13, 11, 13]
      )
    , ( [(G, 1), (C, 3), (Ef, 4)] -- i
      , [ [(0, mord 1), (5, mord 1), (-1, sub (A, 1))]
        , [(8, sub (Bf, 2)), (10, sub (D, 3)), (-10, sub (Bf, 2)), (-8, sub (D, 3))]
        , [(0, appog 2), (2, mord 2), (-3, mord 2), (3, sub (F, 4)), (6, sub (F, 4)), (9, appog 2)]
        ]
      , generateRhythms3 (3 * 24) 2 [9, 9, 9] 1 [9, 13, 9] 0 [13, 11, 13]
      )
    , ( [(Af, 1), (C, 3), (Ef, 4)] -- VI
      , [ [(0, mord 2), (6, mord 2), (-2, sub (Bf, 1))]
        , [(4, sub (Bf, 3)), (-4, mord 2)]
        , [(2, sub (D, 4)), (5, sub (D, 4)), (-2, sub (E, 4)), (-1, sub (E, 4))]
        ]
      , generateRhythms3 (3 * 24) 0 [9, 9, 9] 1 [15, 13] 2 [13, 11, 13]
      )
    , ( [(Af, 1), (C, 3), (F, 4)] -- IV (climax)
      , [ [(0, mord 2), (4, mord 2), (7, mord 2), (-2, sub (Bf, 1))]
        , [(3, mord 1), (-3, mord 1)]
        , [ (1 , sub (G, 4))
          , (4 , sub (G, 4))
          , (5 , sub (Af, 4))
          , (6 , sub (Af, 4))
          , (7 , sub (Af, 4))
          , (9 , sub (G, 4))
          , (-5, sub (Ef, 4))
          ]
        ]
      , generateRhythms3 (3 * 24) 0 [9, 9, 9] 0 [15, 15, 15] 1 [13, 15, 13]
      )
    , ( [(Af, 1), (C, 3), (E, 4)] -- passthrough
      , [[(0, mord 2), (4, mord 2), (-2, sub (G, 1))], [(5, sub (Bf, 2))], [(2, sub (F, 4))]]
      , generateRhythms3 (1 * 24) 0 [3] 0 [10] 1 [9]
      )
            -- Section 3
    , ( [(G, 1), (C, 3), (E, 4)] -- I = V/f
      , [[(-3, sub (Af, 1))], [(0, appog (-2))], [(5, mord 2)]]
      , generateRhythms3 (3 * 24) 0 [9, 9, 9] 0 [15, 13, 11] 1 [11, 9, 7]
      )
    , ( [(Gf, 1), (Bf, 2), (Df, 4)] -- bII
      , [[(-2, sub (Af, 1))], [(0, sub (C, 3))], [(1, appog (-1)), (6, mord 2)]]
      , generateRhythms3 (2 * 24) 0 [9, 3] 0 [15, 9] 0 [11, 11]
      )
    , ( [(Df, 2), (Gf, 2), (Bf, 3)] -- bII
      , [[(-2, sub (Ef, 2))], [(0, appog 1)], [(1, appog 2), (3, mord 2)]]
      , generateRhythms3 (1 * 24) 0 [9] 0 [13] 1 [11]
      )
    , ( [(C, 2), (F, 2), (Af, 3)] -- i 6/4
      , [[(-2, sub (Bf, 1))], [(0, mord 2), (-10, mord 2)], [(0, sub (Bf, 3)), (4, mord 2), (-5, mord 2)]]
      , generateRhythms3 (2 * 24) 0 [1, 9] 0 [10, 10, 10] 1 [11, 11, 11]
      )
    , ( [(C, 2), (E, 2), (G, 3)] -- V
      , [[(-2, sub (Df, 2))], [(0, sub (F, 2)), (2, mord (-2)), (-1, mord (-2))], []]
      , generateRhythms3 (4 * 24) 0 [9, 6, 6] 1 [10, 15, 10] 2 [11, 11, 11]
      )
    , ( [(F, 1), (F, 2), (Af, 3)] -- i
      , [[], [(5, appog (-1)), (10, appog (-1))], [(-2, sub (Bf, 3)), (-1, sub (C, 4))]]
      , generateRhythms3 (3 * 24) 0 [6, 3, 3] 0 [10, 9, 8] 1 [9, 8, 7]
      )
            -- Coda
    , ( [(F, 1), (F, 2), (C, 4)] -- i
      , [[], [(5, sub (C, 1))], [(-2, sub (F, 3)), (-1, sub (C, 3))]]
      , generateRhythms3 (5 * 24) 0 [3, 3, 1, 1] 0 [10, 5, 1] 1 [11, 6, 1]
      )
    ]
