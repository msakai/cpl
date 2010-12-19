module Funct
    ( Funct(..)
    ) where

import Variance

class Funct f where
    variance :: f -> [Variance]
