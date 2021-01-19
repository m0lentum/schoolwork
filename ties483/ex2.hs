-- TIES483 exercise 2, Mikael Myyrä

module Ex2 where

-- (1.)
-- Assume that you are buying and selling electricity. The more electricity you
-- buy, the better price you get per kwH and the buying prize of electricity
-- follows function f(x) = 1-0.01x, where 0<=x<=50 is the amount of electricity
-- you buy. On the other hand, the price that you get from selling electricity
-- follows function g(x) = 2-0.01x^2 with x again the amount of electricity that
-- you sell. Formulate an optimization problem that maximizes the profit.

-- Answer (1.)
-- Objective function: difference between price of sold and bought electricity.
-- Assuming we don't buy and store excess in batteries, i.e. we sell all we buy.
-- Negative to fit the form where the optimal solution is the minimum of f
-- obj(x) = f(x) - g(x)
--        = 1 - 0.01x - (2 - 0.01x^2)
--        = 0.01x^2 - 0.01x - 1
-- Constraints: max and min amount of electricity we can buy
-- Both are inequality constraints.
--      g1(x) = x        (x >= 0)
--      g2(x) = 50 - x   (x <= 50)
-- Formal problem using previous definitions:
--      min   obj(x)
--      s.t.  gj(x) >= 0 for j \in 1, 2
--            x \in R

-- (2.)
-- Use bisection search to optimize problem
--     min (1-x)^2+x
--     s.t. x \in [0,2]
-- Code the search algorithm by yourself, not use the one shown in the lecture
-- material.

-- using Writer to log info about steps
import Control.Monad.Writer

bisectionSearchEx2 :: Float -> Writer [String] Float
bisectionSearchEx2 accuracy =
  let objectiveFn x = (1 - x) ^ 2 + x
      initialRange = (0, 2)
      epsilon = 1e-5

      doSearch :: (Float, Float) -> Writer [String] Float
      doSearch (rangeMin, rangeMax)
        -- we've reached the accuracy we wanted, stop
        | rangeMax - rangeMin < accuracy * 2.0 = do
          tell ["Returning " ++ show midpoint ++ " (objective fn value " ++ show (objectiveFn midpoint) ++ ")"]
          return midpoint
        -- function is increasing at the midpoint, so the minimum is before that point
        | objectiveFn midpoint < objectiveFn (midpoint + epsilon) = do
          logStep (rangeMin, midpoint)
          doSearch (rangeMin, midpoint)
        -- function is decreasing at the midpoint, so the minimum is after that point
        | otherwise = do
          logStep (midpoint, rangeMax)
          doSearch (midpoint, rangeMax)
        where
          midpoint = (rangeMin + rangeMax) / 2.0
   in doSearch initialRange

logStep :: (Float, Float) -> Writer [String] ()
logStep (rangeMin, rangeMax) =
  tell ["Stepping to [" ++ show rangeMin ++ ", " ++ show rangeMax ++ "]"]

-- (3.)
-- Use golden section search to optimize the problem from above exercise. Code
-- the search algorithm by yourself, not use the one shown in the lecture
-- material.

goldenSectionSearchEx3 :: Float -> Writer [String] Float
goldenSectionSearchEx3 accuracy =
  let objectiveFn x = (1 - x) ^ 2 + x
      initialRange = (0, 2)
      goldenSection = (sqrt 5 - 1) / 2.0

      doSearch :: (Float, Float) -> Maybe (Either Float Float) -> Writer [String] Float
      doSearch (rangeMin, rangeMax) cachedPointValue
        -- we've reached the accuracy we wanted, stop
        | rangeMax - rangeMin < accuracy * 2.0 = do
          tell ["Returning " ++ show midpoint ++ " (objective fn value " ++ show (objectiveFn midpoint) ++ ")"]
          return midpoint
        -- left point is lower than right, keep it in the search interval
        | objectiveFn leftGSPoint < objectiveFn rightGSPoint = do
          logStep (rangeMin, rightGSPoint)
          doSearch (rangeMin, rightGSPoint) (Just $ Left valueAtLeftPoint)
        -- right point is lower than left, ...
        | otherwise = do
          logStep (leftGSPoint, rangeMax)
          doSearch (leftGSPoint, rangeMax) (Just $ Right valueAtRightPoint)
        where
          midpoint = (rangeMin + rangeMax) / 2.0
          range = rangeMax - rangeMin
          leftGSPoint = rangeMax - goldenSection * range
          rightGSPoint = rangeMin + goldenSection * range
          -- using the fact that after a golden section step
          -- the point that stays inside our interval still divides it by the golden section
          -- to cache the value at that point for the next iteration
          -- and avoid an evaluation of the objective function
          valueAtLeftPoint =
            case cachedPointValue of
              -- right point of previous iteration is now the left point
              Just (Right v) -> v
              _ -> objectiveFn leftGSPoint
          -- same as above, mirrored
          valueAtRightPoint =
            case cachedPointValue of
              Just (Left v) -> v
              _ -> objectiveFn rightGSPoint
   in doSearch initialRange Nothing

-- (4.)
-- Use differentiation/derivation to optimize the optimization problem of task 1
-- (above) and to verify the answer of optimization problem in task 2 (above).

-- Answer:
-- objective function f(x) = (1-x)^2 + x
--                         = x^2 - x + 1
-- its derivative f'(x) = 2x - 1
-- f'(x) = 0  <=>  2x - 1 = 0  <=>  x = 1/2
-- f(x) is an upwards opening parabola, therefore this is its minimum.
-- Value of the objective function at this point is:
-- f(1/2) = (1/2)^2 + 1/2 = 3/4
-- Looks like my algorithms are converging on 0.5, so they're probably correct.

main :: IO ()
main =
  let accuracies = [1.0, 0.1, 0.01, 0.0001, 1e-7]
   in do
        putStrLn "Bisection search results:"
        mapM_
          ( \acc -> do
              putStrLn $ "Accuracy " ++ show acc ++ ": "
              mapM_ putStrLn (execWriter $ bisectionSearchEx2 acc)
          )
          accuracies
        putStrLn "---------------------------------------------"
        putStrLn "Golden section search results:"
        mapM_
          ( \acc -> do
              putStrLn $ "Accuracy " ++ show acc ++ ": "
              mapM_ putStrLn (execWriter $ goldenSectionSearchEx3 acc)
          )
          accuracies
