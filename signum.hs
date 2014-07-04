mysignum x =
  if x < 0
     then -1
     else if x > 0
       then 1
        else 0

mysignum2 x =
  case (x < 0) of
    True -> -1
    False -> case (x > 0) of
      True -> 1
      False -> 0

roots a b c =
  ( (-b + sqrt ( b*b - 4 * a * c ) ) / ( 2 * a ), (-b - sqrt ( b*b - 4 * a * c ) ) / ( 2 * a ) )
  
roots2 a b c =
  let d = sqrt ( b*b - 4 * a * c )
      twoa = 2 * a
  in ( (-b + d ) / twoa, (-b - d ) / twoa )

factorial' a 1 = a
factorial' a n = factorial' ( a * n ) ( n - 1 )
factorial n = factorial' 1 n




fib 1 = 1
fib 2 = 1
fib n = fib ( n - 1 ) + fib ( n - 2 )


afib ::  Integer -> Integer -> Integer  -> Integer
afib     0          p1         p2        = p1 + p2
afib     count      p1         p2        = afib ( count - 1 ) p2 ( p1 + p2 )
fib2 1 = 1
fib2 2 = 1
fib2 n = afib ( n - 3 ) ( fib2 1 ) ( fib2 2 )

