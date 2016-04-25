doubleMe x = x + x
doubleUs x y = 2*x + 2*y
squaredDistance x y = x^2 + y^2

doubleSmallNumber x = if x > 100
                        then x
                        else 2*x

doubleSmallNumber' x = (if x  > 100 then x else 2*x) + 1
