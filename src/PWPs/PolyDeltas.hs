{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

{-|
Module      : Deltas
Description : Polynomials extended with delta functions
Copyright   : (c) Peter Thompson, 2024
License     : BSD-2-Clause
Maintainer  : peter.thompson@pnsol.com
Stability   : experimental

We extend the polynomials with delta functions so that we can have 'pieces' of either.
We can add and multiply polynomials and add deltas; multiplying deltas only makes sense
in the context of multiplying CDFs, when it will be OK.
We will only be combining a delta and a poly over a zero interval, where the delta dominates.
We can define the operators in a way that keeps the deltas intact and makes the
piecewise integration/differentiation work.
-}
module PWPs.PolyDeltas
(
      PolyDelta (..)
    , PolyHeaviside (..)
    , polyHeavisideRoot
)
where
import PWPs.ConvolutionClasses
import PWPs.SimplePolynomials as SP
import Debug.Trace

{- |
A PolyDelta either a polynomial, a (shifted, scaled) Delta or a (shifted, scaled) Heaviside. 
A delta has a mass, and a Heaviside has a starting value and a rise; 
for probabilities all should be constrained between 0 and 1. 
The position of both Ds and Hs is stored as its basepoint when doing piecewise operations.
-}
data PolyDelta a = Pd (Poly a) | D a
    deriving (Show)
instance Eq a => Eq (PolyDelta a)
    where
        Pd x == Pd y = x == y
        D x  == D y  = x == y
        Pd _ == D _  = False
        D _  == Pd _ = False
instance Functor PolyDelta where
    fmap f (D x) = D (f x)
    fmap f (Pd x) = Pd (fmap f x)
data PolyHeaviside a = Ph (Poly a) | H a a
    deriving (Show)

instance Eq a => Eq (PolyHeaviside a)
    where
        Ph x  == Ph y = x == y
        H x y == H x' y'  = (x == x') && (y == y')
        Ph _  == H _ _  = False
        H _ _ == Ph _ = False

instance Functor PolyHeaviside where
    fmap f (H x y) = H (f x) (f y)
    fmap f (Ph x) = Ph (fmap f x)

type MyConstraints a = (Eq a, Num a, Fractional a)
type EqNum a = (Eq a, Num a) 
type OrdNumEqFrac a = (Ord a, Num a, Eq a, Fractional a) 
type OrdNumEqFracShow a = (Ord a, Num a, Eq a, Fractional a, Show a) 

plusPD :: (Eq a, Fractional a) => PolyDelta a -> PolyDelta a -> PolyDelta a
-- Polynomials have zero mass at a single point, so they are dominated by Ds and Hs
plusPD (Pd x) (Pd y) = Pd (x + y)
plusPD (Pd _) (D x)  = D x
plusPD (D x) (Pd _)  = D x
plusPD (D x) (D x')  = D (x + x')

timesPD :: (Eq a, Fractional a) => PolyDelta a -> PolyDelta a -> PolyDelta a
timesPD (Pd x) (Pd y) = Pd (x * y)
timesPD (Pd _) (D x)  = D x
timesPD (D x) (Pd _)  = D x
timesPD (D x) (D x')  = D (x * x')

instance MyConstraints a => Num (PolyDelta a) where
    (+)           = plusPD
    (*)           = timesPD
    negate        = fmap negate
    abs           = undefined
    signum        = undefined
    fromInteger n = Pd $ makePoly $ Prelude.fromInteger n


plusPH :: (Eq a, Fractional a) => PolyHeaviside a -> PolyHeaviside a -> PolyHeaviside a
-- Polynomials have zero mass at a single point, so they are dominated by Ds and Hs
plusPH (Ph x) (Ph y)     = Ph (x + y)
plusPH (Ph _) (H x y)    = H x y
plusPH (H x y) (Ph _)    = H x y
plusPH (H x y) (H x' y') = H (x + x') (y + y')

timesPH :: (Eq a, Fractional a) => PolyHeaviside a -> PolyHeaviside a -> PolyHeaviside a
timesPH (Ph x) (Ph y)     = Ph (x * y)
timesPH (Ph _) (H x y)    = H x y
timesPH (H x y) (Ph _)    = H x y
timesPH (H x y) (H x' y') = H (x * x') (y * y')
instance MyConstraints a => Num (PolyHeaviside a) where
    (+)           = plusPH
    (*)           = timesPH
    negate        = fmap negate
    abs           = undefined
    signum        = undefined
    fromInteger n = Ph $ makePoly $ Prelude.fromInteger n

integratePDH :: (Eq a, Fractional a) => PolyDelta a -> PolyHeaviside a
integratePDH (Pd x) = Ph (integratePoly x)
integratePDH (D x)  = H 0 x

differentiatePHD :: MyConstraints a => PolyHeaviside a -> PolyDelta a
differentiatePHD (Ph x)  = Pd (differentiatePoly x)
differentiatePHD (H x y) = D (y - x)

instance MyConstraints a => Integrable (PolyDelta a) (PolyHeaviside a)
    where
        integrate        = integratePDH

instance MyConstraints a => Differentiable (PolyHeaviside a) (PolyDelta a)
    where
        differentiate    = differentiatePHD

scalePD :: EqNum a => a -> PolyDelta a -> PolyDelta a
scalePD x (Pd a) = Pd (SP.scalePoly x a)
scalePD x (D y)  = D (x * y)

evaluatePD :: EqNum a => a -> PolyDelta a -> [a]
evaluatePD point (Pd x) = [SP.evaluatePoly point x]
evaluatePD _ (D x)      = [x]

boostPD :: MyConstraints a => a -> PolyDelta a -> PolyDelta a
boostPD x (Pd y) = Pd y + Pd (makePoly x)
boostPD _ (D y)  = D y
instance MyConstraints a => Evaluable a (PolyDelta a)
    where
        evaluate  = evaluatePD
        boost     = boostPD
        scale     = scalePD

scalePH :: EqNum a => a -> PolyHeaviside a -> PolyHeaviside a
scalePH x (Ph a)  = Ph (SP.scalePoly x a)
scalePH x (H y z) = H (x * y) (x * z)

evaluatePH :: EqNum a => a -> PolyHeaviside a -> [a]
evaluatePH point (Ph x) = [SP.evaluatePoly point x]
evaluatePH _ (H x y)    = [x, y]

boostPH :: MyConstraints a => a -> PolyHeaviside a -> PolyHeaviside a
boostPH x (Ph y) = Ph y + Ph (makePoly x)
boostPH x (H y z) = H (x + y) (x + z)

instance MyConstraints a => Evaluable a (PolyHeaviside a)
    where
        evaluate  = evaluatePH
        boost     = boostPH
        scale     = scalePH

-- | Removes excess basepoints if the objects on either side are the same
aggregate :: Eq a => [(a, PolyDelta a)] -> [(a, PolyDelta a)]
aggregate []    = error "Empty list of polydeltas"
aggregate [x]   = [x] -- need at least two elements to do anything
aggregate ((bx, x):(by, y):xs)
    | x == y    = aggregate $ (bx, x):xs -- throw away the second basepoint
    | otherwise = (bx, x) : aggregate ((by, y):xs)

convolvePolyDeltas :: (Num a, Fractional a, Ord a)
                   => (a, a, PolyDelta a) -> (a, a, PolyDelta a) -> [(a, PolyDelta a)]
{- |
When both arguments are polynomials, we check the intervals are non-zero then use convolvePolys and just map the type.
For a delta, lower == upper (invariant to be checked), and the effect of the delta is to translate the other
argument (whichever it is) along by this amount. Need to ensure there is still an initial interval based at zero.
-}
convolvePolyDeltas (lf, uf, Pd f) (lg, ug, Pd g) =
    if (uf <= lf) || (ug <= lg) then error "Invalid polynomial interval width"
                                else aggregate $ map (\(x, p) -> (x, Pd p)) (convolvePolys (lf, uf, f) (lg, ug, g))
convolvePolyDeltas (lf, uf, D f) (lg, ug, Pd g)
    | lf /= uf     = error "Non-zero delta interval"
    | ug < lg      = error "Negative interval width"
    | f == 0       = [(0, Pd zeroPoly)] -- convolving with a zero-sized delta gives nothing
    | lf == 0      = [(0, scalePD f (Pd g)), (ug, Pd zeroPoly)] -- degenerate case of delta at zero
    -- Shift the poly by the basepoint of the delta and insert a new initial zero interval
    | otherwise    = aggregate [(0, Pd zeroPoly), (lg + lf, scalePD f (Pd (shiftPoly lf g))), (ug + lf, Pd zeroPoly)]
convolvePolyDeltas (lf, uf, Pd f) (lg, ug, D g) = convolvePolyDeltas (lg, ug, D g) (lf, uf, Pd f)  -- commutative
convolvePolyDeltas (lf, uf, D f) (lg, ug, D g) -- both deltas
    | lf /= uf || lg /= ug  = error "Non-zero delta interval"
    | f * g == 0            = [(0, Pd zeroPoly)] -- convolving with a zero-sized delta gives nothing
    | lg + lf == 0          = [(0, D (f * g)), (0, Pd zeroPoly)] -- degenerate case of deltas at zero
    | otherwise             = [(0, Pd zeroPoly), (lg + lf, D (f * g)), (lg + lf, Pd zeroPoly)]

instance (Num a, Fractional a, Ord a) => CompactConvolvable a (PolyDelta a)
    where
        convolveIntervals = convolvePolyDeltas

{-|
    We measure whether or not a polyheaviside is consistently above or below zero, or equals zero
-}
comparePHToZero :: (Fractional a, Eq a, Ord a) => (a, a, PolyHeaviside a) -> Maybe Ordering
comparePHToZero (lf, uf, Ph f) = SP.compareToZero (lf, uf, f) -- simple polynomial case
comparePHToZero (lf, uf, H x y)
    | lf /= uf                                          = error "Non-zero Heaviside interval"
    | (x == 0) && (y == 0)                              = Just EQ
    | ((x > 0) && (y >= 0)) || ((x >= 0) && (y > 0))    = Just GT
    | ((x < 0) && (y <= 0)) || ((x <= 0) && (y < 0))    = Just LT
    | otherwise                                         = Nothing

instance (Fractional a, Eq a, Ord a) => Comparable a (PolyHeaviside a)
    where
        compareZero = comparePHToZero

{-|
    We merge polynomials if they are equal. We merge Ds/Hs by adding them (not that we expect this case).
    We merge a zero D/H with a polynomial by discarding it. Other cases do not merge.
-}
instance (Num a, Eq a, Fractional a) => Mergeable (PolyDelta a)
    where
        mergeObject a b = case (a, b) of
            (Pd x, Pd y) -> if x == y then Just (Pd y) else Nothing
            (D x, Pd y)  -> if x == 0 then Just (Pd y) else Nothing
            (D x, D y)   -> Just (D (x + y))
            (_, _)       -> Nothing
        zero = Pd zeroPoly

instance (Num a, Eq a, Fractional a) => Mergeable (PolyHeaviside a)
    where
        mergeObject a b = case (a, b) of
            (Ph x, Ph y)     -> if x == y then Just (Ph y) else Nothing
            (H x y, Ph z)    -> if x == y then Just (Ph z) else Nothing
            (H x y, H x' y') -> Just (H (x + x') (y + y'))
            (_, _)           -> Nothing
        zero = Ph zeroPoly

{-|
    Given an interval containing a given value of a PolyHeaviside, find its location
-}
polyHeavisideRoot  :: OrdNumEqFrac a => a -> a -> (a, a) -> PolyHeaviside a -> Maybe a
-- If we have a step, the interval is zero width so this is the root
polyHeavisideRoot  _ _ (l, u) (H _ _) = if l /= u then error "Non-zero Heaviside interval" else Just l
-- otherwise we have a polynomial: subtract the value we are looking for so that we seek a zero crossing
polyHeavisideRoot  e x (l, u) (Ph p) = findPolyRoot e (l, u) (p - makePoly x)

displayPolyDelta :: OrdNumEqFrac a => a -> (a, a, PolyDelta a) -> Either (a,a) [(a, a)]
displayPolyDelta _ (l, u, D x)
    | l /= u    = error "Non-zero delta interval"
    | otherwise = Left (l, x)
displayPolyDelta s (l, u, Pd p) 
    | l >= u    = error "Invalid polynomial interval"
    | otherwise = Right (displayPoly p (l, u) s)
instance OrdNumEqFrac a => Displayable a (PolyDelta a)
    where
        displayObject = displayPolyDelta

displayPolyHeaviside  :: OrdNumEqFrac a => a -> (a, a, PolyHeaviside a) -> Either (a,a) [(a, a)]
displayPolyHeaviside  s (l, u, Ph p)  = if l >= u then error "Invalid polynomial interval"
                                    else Right (displayPoly p (l, u) s)
displayPolyHeaviside  _ (l, u, H x y) = if l /= u then error "Non-zero heaviside interval"
                                    else Left (l, y - x)

instance OrdNumEqFrac a => Displayable a (PolyHeaviside a)
    where
        displayObject = displayPolyHeaviside