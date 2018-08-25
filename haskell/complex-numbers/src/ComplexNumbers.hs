module ComplexNumbers
(Complex,
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs)

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex = uncurry Complex

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex r i) = Complex r (negate i)

abs :: Floating a => Complex a -> a
abs (Complex r i) = sqrt $ r * r + i * i

real :: Num a => Complex a -> a
real (Complex r _ ) = r

imaginary :: Num a => Complex a -> a
imaginary (Complex _ i) = i

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex r i) (Complex s j) = Complex (r * s - i * j) (r * j + s * i)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex r i) (Complex s j) = Complex (r + s) (i + j)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex r i) (Complex s j) = Complex (r - s) (i - j)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex r i) (Complex s j) = let d = s*s + j*j in
                                    Complex ((r*s + i*j)/d) ((i*s - r*j)/d)
