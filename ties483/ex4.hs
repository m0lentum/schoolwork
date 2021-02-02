-- TIES483 exercise 4, Mikael Myyrä

module Ex4 where

import Control.Monad.Writer
import Data.List

-- implementing Nelder-Mead method which is used in the relevant course slides
-- following the Wikipedia article https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method
-- using linear algebra defined at the end of the file

nelderMeadSearch :: (Vec2 -> Float) -> Vec2 -> Float -> Vec2
nelderMeadSearch objectiveFn startingPoint terminationThreshold =
  doSearch initialSimplex
  where
    -- fixed initial shape for simplicity
    initialSimplex = [startingPoint, startingPoint .+ Vec2 1.0 0.0, startingPoint .+ Vec2 0.0 1.0]
    reflectionCoef = 1.0
    expansionCoef = 2.0
    contractionCoef = 0.5
    shrinkCoef = 0.5

    doSearch :: [Vec2] -> Vec2
    doSearch simplex
      -- termination condition
      | objVariance <= terminationThreshold ^ 2 =
        bestX
      -- reflected point is second best, loop immediately (step 3)
      | reflectedVal < midVal && reflectedVal >= bestVal =
        doSearch [bestX, midX, reflectedX]
      -- reflected point is best, do expansion (step 4)
      | reflectedVal < bestVal =
        if expandedVal < reflectedVal
          then doSearch [bestX, midX, expandedX]
          else doSearch [bestX, midX, reflectedX]
      -- do contraction, find point better than worst (step 5)
      | contractedVal < worstVal =
        doSearch [bestX, midX, contractedX]
      -- nothing else worked, shrink whole simplex
      | otherwise =
        doSearch shrunkenSimplex
      where
        objectiveVals = fmap objectiveFn simplex
        -- calculate variance for standard deviation -based termination condition
        objMean = sum objectiveVals / 3.0
        objVariance = sum $ fmap (\x -> (x - objMean) ^ 2) objectiveVals
        -- actual algorithm step
        -- (1.) sort
        sortedPointsAndVals = sortOn snd (zip simplex objectiveVals)
        -- although List data type doesn't let us express this at the type level,
        -- we make sure there's always exactly 3 elements so this match will always work
        [(bestX, bestVal), (midX, midVal), (worstX, worstVal)] = sortedPointsAndVals
        -- (2.) compute centroid of non-worst points
        centroid = 0.5 .* (bestX .+ midX)
        -- (3.) reflect
        reflectedX = centroid .+ (reflectionCoef .* (centroid .+ neg worstX))
        reflectedVal = objectiveFn reflectedX
        -- (4.) expand (move reflected away from centroid)
        expandedX = centroid .+ (expansionCoef .* (reflectedX .+ neg centroid))
        expandedVal = objectiveFn expandedX
        -- (5.) contract (move worst towards centroid)
        contractedX = centroid .+ (contractionCoef .* (worstX .+ neg centroid))
        contractedVal = objectiveFn contractedX
        -- (6.) shrink
        shrunkenSimplex =
          [ bestX,
            bestX .+ (shrinkCoef .* (midX .+ neg bestX)),
            bestX .+ (shrinkCoef .* (worstX .+ neg bestX))
          ]

-------------------------------------------------------------
------------------ ACTUAL EXERCISE START --------------------
-------------------------------------------------------------

-- For tasks 1-3, we study optimization problem
--   min    x1^2 + x2^2 + x1 + 2*x2
--   s.t.   x1 + x2 = 1
--          x1, x2 \in R

objectiveFn :: Vec2 -> Float
objectiveFn (Vec2 x1 x2) =
  x1 ^ 2 + x2 ^ 2 + x1 + 2 * x2

eqConstraint :: Vec2 -> Float
eqConstraint (Vec2 x1 x2) =
  x1 + x2

-- (1.)
-- Solve the problem using the penalty function method. Note that it is not
-- sufficient to use a fixed value for r.

penalizedObjective :: Float -> Vec2 -> Float
penalizedObjective penaltyCoef x =
  objectiveFn x + penaltyCoef * (eqConstraint x ^ 2)

penaltySearch :: Vec2 -> Writer [String] Vec2
penaltySearch start = do
  tell ["Starting at " ++ show start ++ " with penalty " ++ show initialPenalty]
  doSearch initialPenalty start
  where
    initialPenalty = 1.0
    -- precision to use with Nelder-Mead
    stepPrecision = 0.001
    -- precision to stop increasing penalty and return
    solutionPrecision = 0.01
    maxConstraintError = 0.01

    doSearch :: Float -> Vec2 -> Writer [String] Vec2
    doSearch penaltyCoef currentPoint
      -- stop if constraints aren't violated too much
      -- and solution hasn't changed too much from previous iteration
      | eqConstraint nextPoint <= maxConstraintError
          && normSq (nextPoint .+ neg currentPoint) <= solutionPrecision ^ 2 = do
        tell ["Returning " ++ show nextPoint]
        return nextPoint
      | otherwise = do
        tell ["Stepping to " ++ show nextPoint ++ " and increasing penalty to " ++ show nextPenalty]
        tell ["Constraint value is " ++ show (eqConstraint nextPoint)]
        doSearch nextPenalty nextPoint
      where
        nextPoint = nelderMeadSearch (penalizedObjective penaltyCoef) currentPoint stepPrecision
        nextPenalty = penaltyCoef * 10

-- (2.)
-- Solve the problem (i.e., approximate the optimal solution) using the barrier
-- function method. Note that you need to do something a bit clever.

-- We need to convert the equality constraint to inequalities to use barrier functions.
-- Additionally, because both inequalities will rise to infinity from opposite sides,
-- the combined function will give infinity in all of R^2 unless we introduce some
-- "slop" to create an allowed region close to the desired line.

ineqConstraints :: [Vec2 -> Float]
ineqConstraints =
  [ \(Vec2 x1 x2) -> x1 + x2,
    \(Vec2 x1 x2) -> - (x1 + x2)
  ]

biggestError :: Vec2 -> Float
biggestError x =
  -- only negative values are errors, so min 0.0
  -- abs to make errors positive for intuitive comparison
  abs $ min 0.0 (minimum $ fmap ($ x) ineqConstraints)

barrieredObjective :: Float -> Float -> Vec2 -> Float
barrieredObjective barrierCoef slop x =
  objectiveFn x + sum (fmap barrierFn ineqConstraints)
  where
    -- addition of the slop factor to allow some error without becoming infinite
    barrierFn constraint =
      1.0 / max 0.0 (constraint x + slop)

barrierSearch :: Vec2 -> Writer [String] Vec2
barrierSearch start = do
  tell ["Starting at " ++ show start ++ " with barrier multiplier " ++ show initialBarrierCoef ++ " and slop " ++ show initialSlop]
  doSearch initialBarrierCoef initialSlop start
  where
    initialBarrierCoef = 1.0
    -- set the slop region to the biggest initial constraint error plus a bit
    -- so that we always start in a non-infinite region
    initialSlop = biggestError start + 0.001
    -- precision to use with Nelder-Mead
    stepPrecision = 0.001
    -- precision to stop increasing penalty and return
    solutionPrecision = 0.01
    maxConstraintError = 0.01

    doSearch :: Float -> Float -> Vec2 -> Writer [String] Vec2
    doSearch barrierCoef slop currentPoint
      -- same termination condition as with the penalty method
      | biggestError nextPoint <= maxConstraintError
          && normSq (nextPoint .+ neg currentPoint) <= solutionPrecision ^ 2 = do
        tell ["Returning " ++ show nextPoint]
        return nextPoint
      | otherwise = do
        tell ["Stepping to " ++ show nextPoint ++ ", decreasing barrier coef to " ++ show nextBarrier ++ " and slop to " ++ show nextSlop]
        tell ["Biggest constraint error is " ++ show (biggestError nextPoint)]
        doSearch nextBarrier nextSlop nextPoint
      where
        nextPoint = nelderMeadSearch (barrieredObjective barrierCoef slop) currentPoint stepPrecision
        nextBarrier = barrierCoef * 0.1
        -- adjust slop with the same rule as at the start
        -- to make sure next step still converges to something
        nextSlop = biggestError nextPoint + 0.001

-- (3.)
-- Solve the problem using projected gradient method. Compare the performance to
-- the penalty function and barrier function methods.

-- (4.)
-- Check the necessary first order KKT conditions for the solution that you found.

-- a few test cases (run with `runhaskell` to see output)
main :: IO ()
main =
  let startingPoints =
        [ Vec2 0.0 0.0,
          Vec2 1.0 1.0,
          Vec2 (-1.0) 2.0,
          Vec2 5.0 (-8.0),
          Vec2 (-15.0) 0.0
        ]
      nmPrecision = 0.001
   in do
        putStrLn "Checking that Nelder-Mead finds the optimum in the unconstrained case:"
        mapM_
          ( \start ->
              let ans = nelderMeadSearch objectiveFn start nmPrecision
               in putStrLn $ "From " ++ show start ++ " NM finds " ++ show ans
          )
          startingPoints
        putStrLn ""
        putStrLn "Penalty method (task 1)"
        mapM_ (\start -> putStrLn "" >> mapM_ putStrLn (execWriter $ penaltySearch start)) startingPoints
        putStrLn ""
        putStrLn "Barrier method (task 2)"
        mapM_ (\start -> putStrLn "" >> mapM_ putStrLn (execWriter $ barrierSearch start)) startingPoints

-- linear algebra definitions

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