roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) =
  let d = sqrt (b * b - 4 * a * c)
      e = 2 * a
  in ( (-b - d) / e, (-b + d) / e )

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a,b) =
  let d = sqrt(a*a + b*b)
  in (a/d, b/d)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (a,b,c) =
  let d = sqrt(a*a + b*b + c*c)
  in (a/d, b/d, c/d)

heron :: (Double, Double, Double) -> Double
heron (a,b,c) = 
  let p = (a+b+c) / 2
  in sqrt(p * (p-a) * (p-b) * (p-c))