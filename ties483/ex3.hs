module Ex3 where

import Control.Monad.Writer

-- defining linear algebra operations here to avoid needing to install a library.
-- actual exercise start is marked with a big comment

data Vec2 = Vec2 Float Float
  deriving (Show)

-- column-major order (for Hessian that's (d^2f/dx1^2, d^2f/dx2dx1, d^2f/dx1dx2, d^2f/dx2^2))
data Mat2 = Mat2 Float Float Float Float
  deriving (Show)

neg :: Vec2 -> Vec2
neg (Vec2 x y) =
  Vec2 (- x) (- y)

-- using squared norm for comparisons to avoid a square root computation
normSq :: Vec2 -> Float
normSq (Vec2 x y) =
  x * x + y * y

(.*) :: Float -> Vec2 -> Vec2
m .* (Vec2 x y) =
  Vec2 (m * x) (m * y)

(.+) :: Vec2 -> Vec2 -> Vec2
(Vec2 x1 y1) .+ (Vec2 x2 y2) =
  Vec2 (x1 + x2) (y1 + y2)

dot :: Vec2 -> Vec2 -> Float
dot (Vec2 x1 y1) (Vec2 x2 y2) =
  x1 * x2 + y1 * y2

mMulV :: Mat2 -> Vec2 -> Vec2
(Mat2 a c b d) `mMulV` v =
  Vec2 (Vec2 a b `dot` v) (Vec2 c d `dot` v)

vTMulM :: Vec2 -> Mat2 -> Vec2
v `vTMulM` (Mat2 a c b d) =
  Vec2 (v `dot` Vec2 a c) (v `dot` Vec2 b d)

fMulM :: Float -> Mat2 -> Mat2
m `fMulM` (Mat2 a c b d) =
  Mat2 (m * a) (m * c) (m * b) (m * d)

(**) :: Mat2 -> Mat2 -> Mat2
(Mat2 a1 c1 b1 d1) ** (Mat2 a2 c2 b2 d2) =
  let row1 = Vec2 a1 b1
      row2 = Vec2 c1 d1
      col1 = Vec2 a2 c2
      col2 = Vec2 b2 d2
   in Mat2 (row1 `dot` col1) (row1 `dot` col2) (row2 `dot` col1) (row2 `dot` col2)

negM :: Mat2 -> Mat2
negM (Mat2 a c b d) =
  Mat2 (- a) (- c) (- b) (- d)

(..+) :: Mat2 -> Mat2 -> Mat2
(Mat2 a1 c1 b1 d1) ..+ (Mat2 a2 c2 b2 d2) =
  Mat2 (a1 + a2) (c1 + c2) (b1 + b2) (d1 + d2)

mat2Identity :: Mat2
mat2Identity =
  Mat2 1 0 0 1

-------------------------------------------------------------
------------------ ACTUAL EXERCISE START --------------------
-------------------------------------------------------------

-- For all of the following problems, we study optimization problem
--      min x1^2 + x2^2 + x1 + 2*x2
--      s.t. x1, x2 \in R

ex3objective :: Vec2 -> Float
ex3objective (Vec2 x1 x2) =
  x1 ^ 2 + x2 ^ 2 + x1 + 2 * x2

-- manually computed gradient function to avoid needing to install a library for automatic differentiation
ex3objGradient :: Vec2 -> Vec2
ex3objGradient (Vec2 x1 x2) =
  Vec2 (2 * x1 + 1) (2 * x2 + 2)

-- same for the inverse Hessian matrix to get a starting point for DFP method
ex3objHessian :: Vec2 -> Mat2
ex3objHessian _ =
  Mat2 2.0 0.0 0.0 2.0

ex3objHessianInv :: Vec2 -> Mat2
ex3objHessianInv _ =
  Mat2 0.5 0.0 0.0 0.5

-- (1.)
-- Implement a steepest descent algorithm, but do a golden section search to
-- each new search direction. In other words, use line search to determine
-- optimal step length. This means that you need to define a single-variable
-- function to determine the step length. Set the length of the interval for
-- doing line search as parameter of the method. Solve above problem using your
-- method.

data StepMethod
  = FixedStep Float
  | GoldenSection Float

steepestDescent :: StepMethod -> Float -> Vec2 -> Writer [String] Vec2
steepestDescent stepMethod precision startingPoint =
  let doSearch :: Vec2 -> Writer [String] Vec2
      doSearch p@(Vec2 x1 x2)
        -- stop if the gradient's norm is close enough to zero
        | normSq gradient <= precision * precision = do
          tell ["Finished at (" ++ show x1 ++ ", " ++ show x2 ++ "), objective fn value " ++ show (ex3objective p)]
          return p
        | otherwise =
          let (nextX1, nextX2) =
                case stepMethod of
                  -- fixed step algorithm for comparison in (2.)
                  FixedStep stepLength ->
                    (x1 - stepLength * dfdx1, x2 - stepLength * dfdx2)
                  GoldenSection searchRange ->
                    let -- use the two-variable objective function value
                        -- at a point `x * gradient` away from current point
                        -- as the objective fn for search
                        searchFn distance =
                          ex3objective $ Vec2 (x1 - distance * dfdx1) (x2 - distance * dfdx2)
                        optimizedDist = goldenSectionSearch searchFn precision (0.0, searchRange)
                     in (x1 - optimizedDist * dfdx1, x2 - optimizedDist * dfdx2)
           in do
                tell ["Stepping to (" ++ show nextX1 ++ ", " ++ show nextX2 ++ ")"]
                doSearch (Vec2 nextX1 nextX2)
        where
          gradient@(Vec2 dfdx1 dfdx2) = ex3objGradient p
   in doSearch startingPoint

goldenSectionSearch :: (Float -> Float) -> Float -> (Float, Float) -> Float
goldenSectionSearch objectiveFn precision initialRange =
  let goldenSection = (sqrt 5 - 1) / 2.0
      -- leaving out the caching and logging from exercise 2 for clarity
      doSearch :: (Float, Float) -> Float
      doSearch (rangeMin, rangeMax)
        -- we've reached the accuracy we wanted, stop
        | rangeMax - rangeMin < precision * 2.0 =
          midpoint
        -- left point is lower than right, keep it in the search interval
        | objectiveFn leftGSPoint < objectiveFn rightGSPoint =
          doSearch (rangeMin, rightGSPoint)
        -- right point is lower than left, ...
        | otherwise =
          doSearch (leftGSPoint, rangeMax)
        where
          midpoint = (rangeMin + rangeMax) / 2.0
          range = rangeMax - rangeMin
          leftGSPoint = rangeMax - goldenSection * range
          rightGSPoint = rangeMin + goldenSection * range
   in doSearch initialRange

-- (2.)
-- Plot the steps of the steepest descent algorithm implemented at the class
-- (with fixed step lengths) against the steps of a steepest descent algorithm
-- that you implemented above. You can choose any starting point you wish, but
-- not the optimum. Compare and analyze the results of both methods.

-- Answer:
-- As shown by my printing experiments in the main function,
-- both methods move in a straight line towards the optimum.
-- A sufficiently high search range for golden section search causes that variant
-- to reach the optimum in one step.
-- I believe this is because the objective function is quadratic with respect to both variables,
-- and therefore the gradient function is linear w.r.t both variables
-- and always points directly away from the (global) optimum.

-- (3.)
-- Implement the Quasi-Newton method with DFP update as described e.g., at
-- https://en.wikipedia.org/wiki/Davidon%E2%80%93Fletcher%E2%80%93Powell_formula.
-- In other words, replace computation of the inverse Hessian in the Newton's
-- method by the DFP update. Solve above problem using this method.

dfp :: Float -> Float -> Vec2 -> Writer [String] Vec2
dfp stepSize precision start =
  let doSearch :: Vec2 -> Mat2 -> Writer [String] Vec2
      doSearch p@(Vec2 x1 x2) invHessian
        | normSq currGradient <= precision * precision = do
          tell ["Finished at (" ++ show x1 ++ ", " ++ show x2 ++ "), objective fn value " ++ show (ex3objective p)]
          return p
        | otherwise = do
          tell ["Stepping to (" ++ show psx1 ++ ", " ++ show psx2 ++ "), approximate inverse Hessian is " ++ show approxNextInvHessian]
          doSearch postStepPoint approxNextInvHessian
        where
          currGradient@(Vec2 dfdx1 dfdx2) = ex3objGradient p
          step = stepSize .* neg (invHessian `mMulV` currGradient)
          postStepPoint@(Vec2 psx1 psx2) = p .+ step
          postStepGradient = ex3objGradient postStepPoint
          -- `y` in the DFP formula
          gradientDiff = neg currGradient .+ postStepGradient
          -- `gamma` in the DFP formula
          gradDiffDotStepInv = 1.0 / dot gradientDiff step
          -- The second term added to the hessian in the DFP formula is a scalar.
          -- Assuming this means adding that scalar times the identity matrix.
          approxNextInvHessian = invHessian ..+ negM ((1.0 / term1Denom) `fMulM` term1Num) ..+ (term2 `fMulM` mat2Identity)
          term1Num = (((invHessian `mMulV` gradientDiff) `dot` gradientDiff) `fMulM` invHessian)
          term1Denom = (gradientDiff `vTMulM` invHessian) `dot` gradientDiff
          term2 = (step `dot` step) / (gradientDiff `dot` step)
   in doSearch start (ex3objHessianInv start)

-- a few test cases (run with `runhaskell` to see output)
main :: IO ()
main =
  let startingPoints =
        [ (0.0, 0.0),
          (1.0, 1.0),
          (-1.0, 2.0),
          (5.0, -8.0),
          (-15.0, 0.0)
        ]
      fixedStepLength = 0.1
      goldenSectionRange = 0.5
      dfpStepSize = 0.5
      precision = 0.01
   in do
        mapM_
          ( \(x1, x2) -> do
              putStrLn $ replicate 60 '='
              putStrLn ""
              putStrLn $ "Starting at (" ++ show x1 ++ ", " ++ show x2 ++ ")"
              putStrLn $ "Fixed step length of " ++ show fixedStepLength ++ " gives:"
              mapM_ putStrLn (execWriter $ steepestDescent (FixedStep fixedStepLength) precision (Vec2 x1 x2))
              putStrLn ""
              putStrLn $ "Golden section search with range " ++ show goldenSectionRange ++ " gives:"
              mapM_ putStrLn (execWriter $ steepestDescent (GoldenSection goldenSectionRange) precision (Vec2 x1 x2))
              putStrLn ""
              -- Newton method gives the same inverse Hessian every time, as expected since it's a constant for this function.
              -- sufficiently large step size gives convergence in one step just like with golden section search,
              -- but with the same step size takes longer than GS to converge (when both take more than one step).
              putStrLn $ "Newton method with DFP Hessian approximation (step size " ++ show dfpStepSize ++ ") gives:"
              mapM_ putStrLn (execWriter $ dfp dfpStepSize precision (Vec2 x1 x2))
              putStrLn ""
          )
          startingPoints