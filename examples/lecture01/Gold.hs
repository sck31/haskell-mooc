module Gold where

-- The golden ratio
phi :: Double
phi = (sqrt 5 + 1) / 2

polynomial :: Double -> Double
polynomial x = x^2 - x - 1


square :: Double -> Double
square x = x*x

f x = polynomial (polynomial x)

f2 = polynomial . polynomial

f3 = polynomial . polynomial . polynomial

main = do
  print (polynomial phi)
  (print . f)  phi
  print (f2 phi)
  print (f3 phi)
