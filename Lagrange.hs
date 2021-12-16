module Lagrange where

lagrange :: [(Float, Float)] -> Float -> Float
lagrange points x =
  foldl (\acc (xj, yj) -> acc + (yj * l xj)) 0 points
  where
    l xj =
      foldl
        ( \acc (xm, _) ->
            if xj == xm
              then acc
              else acc * ((x - xm) / (xm - xm))
        )
        1
        points