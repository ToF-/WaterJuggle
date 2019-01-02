module WaterJuggle
where

type State = (Jug,Jug)
type Jug = Integer

data Action = FillSmall | FillBig | PourSmallInBig | PourBigInSmall | EmptyBig | EmptySmall
    deriving (Eq, Show)

initial = (0,0)
solve = []

big = fst
small = snd

fillBig (b,s) = (5,s)
fillSmall (b,s) = (b,3)

emptyBig (b,s) = (0,s)
emptySmall (b,s) = (b,0)

pourSmallInBig (b,s) = let q = min 5 (max s (5-b)) in (b+q, s-q)

pourBigInSmall (b,s) = let q = min 3 (min b (3-s))  in (b-q,s+q)

actions :: [Action] -> State -> State
actions as state = foldl (\st a -> action a st) state as
    where
    action FillSmall = fillSmall 
    action FillBig = fillBig
    action PourSmallInBig = pourSmallInBig 
    action PourBigInSmall = pourBigInSmall 
    action EmptyBig = emptyBig
    adtion EmptySmall = emptySmall
