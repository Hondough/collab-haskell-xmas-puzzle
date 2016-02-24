module Grid where

data Block = B | W

type Grid = [[Block]]

instance Show Block where
  show B = "1"
  show W = "_"

instance Eq Block where
  B == B = True
  W == W = True
  _ == _ = False

instance Ord Block where
  compare B W = GT
  compare W B = LT
  compare _ _ = EQ
