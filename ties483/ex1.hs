module Ex1 where

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

minimumForGrade :: Int -> Int -> Int
minimumForGrade maxPoints grade =
  0

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
   in do
        sequence_ $
          fmap
            ( \(max, got) ->
                putStrLn $ "Max " ++ show max ++ ", got " ++ show got ++ ", gets " ++ show (approxGrade max got)
            )
            approxGradeTestVals