{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : PWPs
Description : Operations on improper random variables
Copyright   : (c) Peter Thompson, 2023
License     : BSD-2-Clause
Maintainer  : peter.thompson@pnsol.com
Stability   : experimental

We build PWPs as ab abstract datatype on top of piecewise polynomials and deltas. 
PWPs may be represented as PDFs or CDFs. They are polymorphic in a constrained numeric type.

We provide functions to construct PWPs as uniform distributions on an interval; a delta at a point;
and a CDF from a list of point-pairs.

We provide functions to turn an PWP into a set of point-pairs representing either the PDF or CDF.

We define operators for convolution, first-to-finish, all-to-finish and weighted choice.
We also provide an operation to extract the probability mass (<= 1).

Note that we can consistently represent 'bottom' using polynomials and 'top' by including deltas.
-}
module PWPs
(
    PWP 
  , makePDF
  , makeCDF
  , constructUniform
  , constructDelta
  , zeroPoly
  , constructCDF
  , firstToFinish
  , allToFinish
  , probChoice
  , (PWPs.<+>)
  , probMass
  , displayCDF
  , displayPDF
)
where

import PWPs.Piecewise
import PWPs.PolyDeltas
import PWPs.SimplePolynomials (makePoly)

type Distribution a = Pieces a (PolyDelta a)

data PWP a = PDF (Distribution a) | CDF (Distribution a)
    deriving (Eq, Show)

makePDF :: (Ord a, Enum a, Eq a, Fractional a, Num a) => PWP a -> Distribution a
-- | Force an PWP into a PDF by differentiating if necessary
makePDF (PDF x) = x
makePDF (CDF x) = differentiate x

makeCDF :: (Ord a, Enum a, Eq a, Fractional a, Num a) => PWP a -> Distribution a
-- | Force an PWP into a CDF by integrating if necessary
makeCDF (CDF x) = x
-- assume PDFs are 0 at 0
makeCDF (PDF x) = integrate x

constructUniform :: (Ord a, Enum a, Eq a, Fractional a, Num a) => a -> PWP a
-- | Construct a PDF with uniform probability from 0 to the given value
constructUniform x = if x <= 0 then error "Invalid interval"
                     else PDF (makePieces [(0, P (makePoly (1/x))), (x, P zero)])

constructDelta :: (Ord a, Enum a, Eq a, Fractional a, Num a) => a -> PWP a
-- | Construct a PDF that is a delta function at the given value
constructDelta x 
    | x < 0 = error "Invalid value"
    | x == 0 = PDF (makePieces [(0, D 0), (0, P 0)])
    | otherwise = PDF (makePieces [(0, P 0), (x, D x), (x, P 0)])

-- | Polynomial with zero value everywhere
zeroPoly :: (Ord a, Enum a, Eq a, Fractional a, Num a) => PWP a
zeroPoly = PDF (makePieces [(0, P $ makePoly 0)])

constructCDF :: (Ord a, Enum a, Eq a, Fractional a, Num a) => [(a, a)] -> PWP a
-- | Construct a CDF from a list of values, treating each new value as a step up from the one before, assuming we start at 0
constructCDF xs 
    | head xs /= (0,0)              = error "CDF fails to start at origin"
    | not (monotonic basepoints)    = error "Basepoints not monotonic"
    | not (monotonic probabilities) = error "Probabilities not monotonic"
    | last probabilities > 1        = error "Probability exceeds one"
    -- First interval is a zero polynomial: subsequent intervals start with a delta and then
    -- have a constant polynomial
    | otherwise = (CDF . makePieces) (interleave treads risers ++ [last treads])
        where
            basepoints = map fst xs
            probabilities = map snd xs
            -- Each step up corresponds to a delta of the difference with the previous value
            risers = zip (tail basepoints) (map D (zipWith (-) (tail probabilities) probabilities))
            treads = zip basepoints (map (P . makePoly) probabilities)
            interleave :: [b] -> [b] -> [b]
            interleave [] _ = []
            interleave _ [] = []
            interleave (x':xs') (y':ys') = x':y':interleave xs' ys'

displayCDF :: (Ord a, Enum a, Eq a, Fractional a, Num a) => Int -> PWP a -> [(a, a)]
-- | Turn an PWP into a list of point pairs corresponding to the CDF, with a given minimum number of points
displayCDF n x = if n <= 0 then error "Invalid number of points" else decomposePWP n (makeCDF x)

displayPDF :: (Ord a, Enum a, Eq a, Fractional a, Num a) => Int -> PWP a -> [(a, a)]
-- | Turn an PWP into a list of point pairs corresponding to the PDF, with a given minimum number of points
displayPDF n x = if n <= 0 then error "Invalid number of points" else decomposePWP n (makePDF x)
    
decomposePWP :: (Ord a, Num a, Fractional a) => Int -> Distribution a -> [(a, a)]
decomposePWP numPoints ys = zip basepoints (map (`evaluateAtApoint` ys) basepoints) 
    where
        pointsList = getPieces ys
        originalBasepoints = map basepoint pointsList
        spacing = last originalBasepoints / Prelude.fromIntegral numPoints
        basepoints = reverse (makePoints originalBasepoints spacing (head originalBasepoints))
        makePoints :: (Ord b, Num b) => [b] -> b -> b -> [b]
        -- make points by repeatedly adding spacing to last interval boundary until it exceeds the next interval boundary
        -- use : for efficiency and then reverse
        makePoints [] _ _ = [] -- already got the last interval boundary included, no need to go further
        makePoints (y':ys') sp prev = 
            if new >= y' 
                then y':makePoints ys' sp y' -- gone past the next interval, so take that instead and carry on
                else new:makePoints (y':ys') sp new -- just add spacing and repeat until we pass the boundary
            where 
                new = prev + sp

firstToFinish :: (Ord a, Enum a, Eq a, Fractional a, Num a) => PWP a -> PWP a -> PWP a
firstToFinish x y = CDF (cdfOfx `plus` cdfOfy `plus` minus (cdfOfx `times` cdfOfy))
    where
        cdfOfx = makeCDF x
        cdfOfy = makeCDF y

allToFinish :: (Ord a, Enum a, Eq a, Fractional a, Num a) => PWP a -> PWP a -> PWP a
allToFinish x y = CDF (makeCDF x `times` makeCDF y)

probChoice :: (Ord a, Enum a, Eq a, Fractional a, Num a) => (a, PWP a) -> (a, PWP a) -> PWP a
{- | 
We can do this on either PDFs or CDFs; if we have CDFs deliver a CDF, if we have both PDFs or one of each, deliver a PDF.
We use weights: these don't have to be the BaseType but this avoids conversions
-}
probChoice (wx, x) (wy, y) = 
    case (x,y) of
        (CDF a, CDF b) -> CDF ((xprob >< a) `plus` (yprob >< b))
        _              -> PDF ((xprob >< makePDF x) `plus` (yprob >< makePDF y))
        where
            xprob = wx/(wx + wy)
            yprob = wy/(wx + wy)

-- | To convolve, force into PDFs and then invoke piecewise convolution
infix 7 <+> -- same as *
(<+>) :: (Ord a, Enum a, Eq a, Fractional a, Num a) => PWP a -> PWP a -> PWP a
x <+> y = PDF (makePDF x PWPs.Piecewise.<+> makePDF y)

probMass :: (Ord a, Enum a, Eq a, Fractional a, Num a) => PWP a -> a
-- | Just extract the final value of the CDF
probMass = piecesFinalValue . makeCDF
