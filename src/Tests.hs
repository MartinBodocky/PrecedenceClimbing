module Tests where

import Debug.Trace
import PrecedenceClimbing

testingPrecedenceClimbing :: Bool
testingPrecedenceClimbing = all (\x->
    let comp = (compute $ fst x) == snd x
        in
        trace ("testingPrecedenceClimbing: Testing... "++ show x) $
        if comp then comp
            else error $ "Unit test failed on "++ show x
        ) unitTestsPrecedenceClimbing

unitTestsPrecedenceClimbing :: [(String,Double)]
unitTestsPrecedenceClimbing = [
    ("1+2",3),
    ("2*3",6),
    ("3-1",2),
    ("4/2",2),
    ("(1+2)+3",6),
    ("1+(2+3)",6),
    ("(2*3)*4",24),
    ("2*(3*4)",24),
    ("(5-2)-1",2),
    ("5-(2-1)",4),
    ("(24/4)/2",3),
    ("24/(4/2)",12),
    ("(2*3)+4",10),
    ("2*(3+4)",14),
    ("(51-(2+3)*(4+5))/2",3),
    ("2^2",4),
    ("(2^3)^2",64),
    ("2^(3^2)",512),
    ("2^3^2",512),
    ("24/4/2",3),
    ("2*3*4",24),
    ("((2*3)*2)-2",10),
    ("(-33^2+2)*(42-2^3)^3/2",2.1440332e7),
    ("(33^2+2)*(42-2^3)^3/2",2.1440332e7)]

