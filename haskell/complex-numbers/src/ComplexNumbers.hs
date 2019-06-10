module ComplexNumbers where

import           Prelude hiding (abs, div)

-- Data definition -------------------------------------------------------------
data Complex a = Complex {real :: a , imaginary :: a} deriving (Eq, Show)

complex :: (a, a) -> Complex a
complex (x,y) = Complex x y

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate z = Complex (real z) (- imaginary z)

abs :: Floating a => Complex a -> a
abs z = sqrt (real z ** 2 + imaginary z ** 2)

complexExp :: Floating a => Complex a -> Complex a
complexExp z = Complex (exp x) 0 `mul` Complex (cos y) (sin y)
  where
    x = real z
    y = imaginary z

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul z w = Complex (a * c - b * d) (b * c + a * d)
  where
    a = real z
    b = imaginary z
    c = real w
    d = imaginary w

add :: Num a => Complex a -> Complex a -> Complex a
add z w = Complex (real z + real w) (imaginary z + imaginary w)

sub :: Num a => Complex a -> Complex a -> Complex a
sub z w = Complex (real z - real w) (imaginary z - imaginary w)

div :: Floating a => Complex a -> Complex a -> Complex a
div z w = mul z (conjugate w `mul` Complex (1/(real w ** 2 + imaginary w ** 2)) 0)
