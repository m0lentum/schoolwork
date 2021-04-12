f :: Float -> Float -> Float
f x y =
  x * exp (-y)

euler :: Float -> Float
euler h =
  step 0.0 0.0
  where
    step x y =
      if x < 2.0 then
        step (x + h) (y + h * f x y)
      else
        y

improvedEuler :: Float -> Float
improvedEuler h =
  step 0.0 0.0
  where
    step x y =
      if x < 2.0 then
        step (x + h) (y + h * (f x y + f (x + h) (y + h * f x y)) / 2.0)
      else
        y

rk4 :: Float -> Float
rk4 h =
  step 0.0 0.0
  where
    step x y =
      if x < 2.0 then
        let
          p = f x y
          q = f (x + h / 2.0) (y + h * p / 2.0)
          r = f (x + h / 2.0) (y + h * q / 2.0)
          s = f (x + h) (y + h * r)
        in step (x + h) (y + h * (p + 2*q + 2*r + s) / 6.0)
      else
        y

main :: IO ()
main = do
  putStrLn (show $ euler 0.2)
  putStrLn (show $ euler 0.1)
  putStrLn (show $ improvedEuler 0.2)
  putStrLn (show $ rk4 0.2)
  
