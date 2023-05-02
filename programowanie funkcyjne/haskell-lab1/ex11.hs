roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e ) {
  d = sqrt (b * b - 4 * a * c)
  e = 2 * a   
}