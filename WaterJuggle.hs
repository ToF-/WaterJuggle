module WaterJuggle
where

type State = (Jug,Jug)
type Jug = Integer
initial = (0,0)
solve = []

big = fst
small = snd

fillBig (b,s) = (5,s)
fillSmall (b,s) = (b,3)

emptyBig (b,s) = (0,s)
emptySmall (b,s) = (b,0)

pourSmallInBig (b,s) = (min 5 b+s,max 0 (s-(b-s)))

pourBigInSmall (b,s) = (max 0 b-(3-s),s+b)
