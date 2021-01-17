-- TIES483 exercise 1, Mikael Myyrä
module Ex1 where

import GHC.Float as F

-- (1.)
-- Implement on a language of your choice a function that calculates a
-- continuous approximation of the grade of this course (grading rules given in
-- the first lecture). That is, the function takes as input the number of points
-- received in the course and the maximal number of points and gives as output a
-- value so that the integer part of that is the grade.

-- for validating inputs as required in (2.)
data GradeResult
  = Grade Float
  | RangeError -- a negative number was entered
  | OrderError -- received points were above maximum
  deriving (Show)

approxGrade :: Float -> Float -> GradeResult
approxGrade maxPoints gotPoints
  | maxPoints < gotPoints = OrderError
  | maxPoints < 0.0 || gotPoints < 0.0 = RangeError
  | otherwise =
    Grade $
      let -- between 0 and 1 due to previous checks
          ratio = gotPoints / maxPoints
          -- a linear approximation where 50% gives 0 and 90% gives 5
          -- (found by solving the system of equations
          --    k*0.5+b = 1
          --    k*0.9+b = 5 )
          linear = 10.0 * ratio - 4.0
       in -- clamp between 0 (failed) and 5
          max 0.0 (min 5.0 linear)

-- (2.)
-- Make a script that finds the minimum number of points that gives a given
-- grade. That is, your script should take as input the maximal numbef of points
-- and the desired grade. Your solution has to call the function defined in task
-- 1 above. Add also checking for incorrect input.

data MinimumResult
  = Minimum Int
  | -- wanted grade was less than zero or more than five
    -- or max points were negative
    RangeError_
  deriving (Show)

-- Fixed step linear search to find the minimum number of points using `approxGrade`.
--
-- Note: it was not said in the assignment whether fractional points are given.
-- Assuming only integer values of points for simplicity.
minimumForGrade :: Int -> Int -> MinimumResult
minimumForGrade maxPoints wantedGrade
  | maxPoints < 0 || wantedGrade < 0 || wantedGrade > 5 = RangeError_
  | otherwise =
    let getsWantedGradeOrMore :: Int -> Bool
        getsWantedGradeOrMore points =
          case approxGrade (F.int2Float maxPoints) (F.int2Float points) of
            -- leaving this match incomplete as the other cases shouldn't ever happen.
            -- this will cause the program to crash if I've made a mistake that causes an error
            -- to be returned from approxGrade
            Grade grade -> grade >= F.int2Float wantedGrade

        -- recursively find the first value that satisfies our condition
        doSearch :: Int -> Int
        doSearch val
          -- this should always be hit before we go over maxPoints
          -- because we've checked that wantedGrade is in the legal range
          -- -> no need to check for val >= maxPoints
          | getsWantedGradeOrMore val = val
          | otherwise = doSearch (val + 1)
     in Minimum $ doSearch 0

-- run through a few example values
main :: IO ()
main =
  let approxGradeTestVals =
        [ (0.0, -1.0),
          (5.0, 10.0),
          (20.0, 6.0),
          (20.0, 10.0),
          (20.0, 15.0),
          (40.0, 25.0),
          (50.0, 42.0),
          (50.0, 45.0),
          (50.0, 50.0)
        ]

      minimumTestVals =
        [ (-1, 0),
          (5, 8),
          (5, -10),
          (10, 1),
          (15, 0),
          (15, 5),
          (40, 3),
          (50, 2)
        ]
   in do
        putStrLn "Approximate grades:"
        mapM_
          ( \(max, got) ->
              putStrLn $ "Max " ++ show max ++ ", got " ++ show got ++ ", gets " ++ show (approxGrade max got)
          )
          approxGradeTestVals
        putStrLn ""
        putStrLn "Minimum grades:"
        mapM_
          ( \(max, want) ->
              putStrLn $ "Max " ++ show max ++ ", to get a " ++ show want ++ " need " ++ show (minimumForGrade max want) ++ " points"
          )
          minimumTestVals