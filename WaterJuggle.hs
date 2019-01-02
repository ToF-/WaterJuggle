module WaterJuggle
where

type Jug = Integer
type State = (Jug, Jug)

data Action = FillSmallJug | FillBigJug | PourSmallIntoBig
    deriving (Eq, Show)

initial = (0,0)

big = fst
small = snd

action :: Action -> State -> State
action FillSmallJug (big,_) = (big,3)
action FillBigJug (_,small) = (5,small)
action PourSmallIntoBig (big, small) = let qty = min (5-big) small in (big+qty, small-qty)

