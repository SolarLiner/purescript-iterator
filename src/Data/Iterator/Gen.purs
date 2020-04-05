module Data.Iterator.Gen where

import Prelude
import Data.Iterator (Iterator, singleton)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf)
