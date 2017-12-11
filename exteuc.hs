-- (x,y,g) s.t. ax+by == gcd, in worst case O(log b)
extEuc :: Integral a => a -> a -> (a, a, a)
extEuc a 0 = (1,0,a)
extEuc a b = (t, s-q*t, abs g) where
    (q,r)   = a `quotRem` b
    (s,t,g) = extEuc b r

-- m s.t. (n*m) mod p = 1, the multiplicative inverse
minverse :: Integral a => a -> a -> a
minverse n p = x `mod` p where
    (x,_,_) = extEuc n p
