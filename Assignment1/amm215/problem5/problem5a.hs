--Problem 5
--PART A)
doubleTheVal :: Double -> Double
doubleTheVal x = 2*x

--version 1 using haskells lamda expressions
compose3 :: (Double -> Double) -> (Double -> Double) -> (Double -> Double) -> (Double -> Double)
compose3 f g h = (\x y z q-> (x (y (z q)))) f g h -- equivelant to lamda expression \f.\g.\h.\x.(f(g(h(x)))) where \ is lamda NOTE here we are passing in 3 of the 4 parameters so the lamda expression returns a funcion

-- version 2 using inner function, definitly easier to read
compose3b :: (Double -> Double) -> (Double -> Double) -> (Double -> Double) -> (Double -> Double)
compose3b f g h = compose
  where
    compose x = f (g (h x))
