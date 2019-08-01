module Grammar
    ( DetGrammar(..)
    , detGenerate
    )
where

import Euterpea
import Utility
import Euclid
import Data.List (sort, groupBy, find)
import Data.Maybe (fromMaybe)
import System.Random

-- Deterministic Grammar

data DetGrammar a = DetGrammar a -- start sentence
        [(a, [a])] -- production rules
        deriving Show

detGenerate :: Eq a => DetGrammar a -> Int -> [a]
detGenerate (DetGrammar st ps) n = iterate (concatMap f) [st] !! n where f a = fromMaybe [a] (lookup a ps)

-- Stochastic Grammar

type Prob = Double
type Rand = Double

data Grammar a = Grammar a -- start sentence
        (RuleSet a) -- production rules
        deriving Show

data RuleSet a = Uni [Rule a]
    | Sto [(Rule a, Prob)]
    deriving (Eq, Ord, Show)

data Rule a = Rule {lhs :: a, rhs :: a}
    deriving (Eq, Ord, Show)

type ReplaceFunc a = [[(Rule a, Prob)]] -> (a, [Rand]) -> (a, [Rand])

gen :: Ord a => ReplaceFunc a -> Grammar a -> Int -> [a]
gen f (Grammar s rules) seed =
    let
        Sto newRules = toStoRules rules
        rands        = randomRs (0.0, 1.0) (mkStdGen seed)
    in if checkProbs newRules then generate f newRules (s, rands) else error "gen: stochastic rule-set is malformed"

toStoRules :: (Ord a, Eq a) => RuleSet a -> RuleSet a
toStoRules (Sto rs) = Sto rs
toStoRules (Uni rs) = Sto (concatMap insertProb rs') where rs' = groupBy (\r1 r2 -> lhs r1 == lhs r2) (sort rs) -- use sameLhs'

insertProb :: [a] -> [(a, Prob)]
insertProb rules = zip rules (repeat probability) where probability = 1.0 / fromIntegral (length rules)

checkProbs :: (Ord a, Eq a) => [(Rule a, Prob)] -> Bool
checkProbs rules = all checkSum $ groupBy sameLhs $ sort rules

epsilon = 0.001

checkSum :: [(Rule a, Prob)] -> Bool
checkSum rules = abs (1.0 - mySum) <= epsilon where mySum = sum $ map snd rules

sameLhs' :: Eq a => Rule a -> Rule a -> Bool
sameLhs' r1 r2 = lhs r1 == lhs r2

sameLhs :: Eq a => (Rule a, Prob) -> (Rule a, Prob) -> Bool
sameLhs (r1, _) (r2, _) = sameLhs' r1 r2

generate :: Eq a => ReplaceFunc a -> [(Rule a, Prob)] -> (a, [Rand]) -> [a]
generate f rules xs =
    let
        newRules = map probDist (groupBy sameLhs rules)
        probDist rrs = let (rs, ps) = unzip rrs in zip rs (tail (scanl (+) 0 ps))
    in map fst (iterate (f newRules) xs)

-- Grammar

data LSys a = N a
    | LSys a :+ LSys a
    | LSys a :. LSys a
    | Id
    deriving (Eq, Ord, Show)

replaceFunc :: Eq a => ReplaceFunc (LSys a)
replaceFunc rules (s, rands) = case s of
    N x -> (getNewRhs rules (N x) (head rands), tail rands)
    a :+ b ->
        let
            (a', rands' ) = replaceFunc rules (a, rands)
            (b', rands'') = replaceFunc rules (b, rands')
        in (a' :+ b', rands'')
    a :. b ->
        let
            (a', rands' ) = replaceFunc rules (a, rands)
            (b', rands'') = replaceFunc rules (b, rands')
        in (a' :. b', rands'')
    Id -> (Id, rands)

getNewRhs :: Eq a => [[(Rule a, Prob)]] -> a -> Rand -> a
getNewRhs ruleSets left rand =
    let
        loop ((r, p) : rs) = if rand <= p then rhs r else loop rs
        loop []            = error "getNewRhs: rand exceeds probabilities"
    in case find (\((r, p) : _) -> lhs r == left) ruleSets of
        Just rules -> loop rules
        Nothing    -> error "getNewRhs: could not find rule for lhs"

-- Interpretation

type InterpRules a b = [(a, Music b -> Music b)]

interpret :: Eq a => LSys a -> InterpRules a b -> Music b -> Music b
interpret (N x) r m = case lookup x r of
    Just f  -> f m
    Nothing -> error "interpret: no interpretation rule"
interpret (a :+ b) r m = interpret a r m :+: interpret b r m
interpret (a :. b) r m = interpret a r $ interpret b r m
interpret Id       r m = m

-- Rules

data LFun = X | Y | Inc | Dec
    deriving (Eq, Ord, Show)

interpRules :: InterpRules LFun Pitch
interpRules = [(X, id), (Y, id), (Inc, transpose 1), (Dec, transpose (-1))]

x, y, inc, dec :: LSys LFun
x = N X
y = N Y
inc = N Inc
dec = N Dec

g1 = Grammar
    x
    (Uni
        [ Rule x (y :+ inc :. ((x :+ (dec :. x)) :+ dec :. (y :+ dec :. (dec :. (y :+ x)) :+ inc :. x)))
        , Rule y   (y :+ y)
        , Rule inc inc
        , Rule dec dec
        ]
    )

t1 n = interpret (gen replaceFunc g1 42 !! n) interpRules (a 3 qn)
