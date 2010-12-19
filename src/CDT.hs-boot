module CDT where

import Variance

data CDT

functName     :: CDT -> String
functVariance :: CDT -> [Variance]
_eqCDT        :: CDT -> CDT -> Bool
