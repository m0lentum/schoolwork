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

bisectionSearchEx2 :: Float -> Float
bisectionSearchEx2 quality =
  let objectiveFn x = (1 - x) ^ 2 + x
      initialRange = (0, 2)
      epsilon = 1e-5

      doSearch :: (Float, Float) -> Float
      doSearch (rangeMin, rangeMax)
        -- we've reached the accuracy we wanted, stop
        | rangeMax - rangeMin < quality * 2.0 = midpoint
        -- function is increasing at the midpoint, so the minimum is before that point
        | objectiveFn midpoint < objectiveFn (midpoint + epsilon) =
          doSearch (rangeMin, midpoint)
        -- function is decreasing at the midpoint, so the minimum is after that point
        | otherwise =
          doSearch (midpoint, rangeMax)
        where
          midpoint = (rangeMin + rangeMax) / 2.0
   in doSearch initialRange

-- (3.)
-- Use golden section search to optimize the problem from above exercise. Code
-- the search algorithm by yourself, not use the one shown in the lecture
-- material.

-- (4.)
-- Use differentiation/derivation to optimize the optimization problem of task 1
-- (above) and to verify the answer of optimization problem in task 2 (above).

main :: IO ()
main =
  let ex2Accuracies = [1.0, 0.1, 0.01, 0.0001, 1e-7]
   in do
        putStrLn "Bisection search results:"
        mapM_ (\acc -> putStrLn $ "Accuracy " ++ show acc ++ ": " ++ show (bisectionSearchEx2 acc)) ex2Accuracies
