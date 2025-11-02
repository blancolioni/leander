x  ==>  x
C x1 .. xN => C x1 .. xN, N >= 0
C p1 .. pN => C x1 .. xN



length [] = 0; length (_:xs) = 1 + length xs
=>
length $x = $x 0 (\_.\xs.+ 1 (length xs))

