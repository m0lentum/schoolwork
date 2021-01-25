module Ex3 where

import Control.Monad.Writer

-- For all of the following problems, we study optimization problem
--      min x1^2 + x2^2 + x1 + 2*x2
--      s.t. x1, x2 \in R

ex3objective :: Float -> Float -> Float
ex3objective x1 x2 =
  x1 ^ 2 + x2 ^ 2 + x1 + 2 * x2

-- manually computed gradient function to avoid needing to install a library for automatic differentiation
-- (this would make checking my answer more difficult for the teacher)
ex3objGradient :: Float -> Float -> (Float, Float)
ex3objGradient x1 x2 =
  (2 * x1 + 1, 2 * x2 + 2)

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

steepestDescent :: StepMethod -> Float -> (Float, Float) -> Writer [String] (Float, Float)
steepestDescent stepMethod precision startingPoint =
  let doSearch :: (Float, Float) -> Writer [String] (Float, Float)
      doSearch (x1, x2)
        -- stop if the gradient's norm is close enough to zero
        | sqrt (dfdx1 * dfdx1 + dfdx2 * dfdx2) <= precision = do
          tell ["Finished at (" ++ show x1 ++ ", " ++ show x2 ++ "), objective fn value " ++ show (ex3objective x1 x2)]
          return (x1, x2)
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
                          ex3objective (x1 - distance * dfdx1) (x2 - distance * dfdx2)
                        optimizedDist = goldenSectionSearch searchFn precision (0.0, searchRange)
                     in (x1 - optimizedDist * dfdx1, x2 - optimizedDist * dfdx2)
           in do
                tell ["Stepping to (" ++ show nextX1 ++ ", " ++ show nextX2 ++ ")"]
                doSearch (nextX1, nextX2)
        where
          (dfdx1, dfdx2) = ex3objGradient x1 x2
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
-- In other words, replace computation of the inverse Hessian in theSolve above
-- problem using this method.

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
      goldenSectionRange = 1.0
      precision = 0.01
   in do
        mapM_
          ( \(x1, x2) -> do
              putStrLn $ "Starting at (" ++ show x1 ++ ", " ++ show x2 ++ ")"
              putStrLn $ "Fixed step length of " ++ show fixedStepLength ++ " gives:"
              mapM_ putStrLn (execWriter $ steepestDescent (FixedStep fixedStepLength) precision (x1, x2))
              putStrLn $ "Golden section search with range " ++ show goldenSectionRange ++ " gives:"
              mapM_ putStrLn (execWriter $ steepestDescent (GoldenSection goldenSectionRange) precision (x1, x2))
              putStrLn ""
          )
          startingPoints