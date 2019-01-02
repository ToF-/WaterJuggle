module WaterJuggle
where

type Jug = Integer
type State = (Jug, Jug)

data Action = FillSmallJug | FillBigJug | PourSmallIntoBig | PourBigIntoSmall | EmptySmallJug | EmptyBigJug
    deriving (Eq, Show)

initial = (0,0)

big = fst
small = snd

totalWater (big,small) = big + small
action :: Action -> State -> State
action FillSmallJug (big,_) = (big,3)
action FillBigJug (_,small) = (5,small)
action PourSmallIntoBig (big, small) = let qty = min (5-big) small in (big+qty, small-qty)
action PourBigIntoSmall (big, small) = let qty = min (3-small) big in (big-qty, small+qty)
action EmptySmallJug (big, _) = (big, 0)
action EmptyBigJug (_,small) = (0, small)


