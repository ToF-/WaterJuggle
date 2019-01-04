module WaterJuggle
where

type Jug = Integer
type State = (Jug, Jug)

data Action = Initialize | FillSmallJug | FillBigJug | PourSmallIntoBig | PourBigIntoSmall | EmptySmallJug | EmptyBigJug
    deriving (Eq, Show, Enum)

type Path = [Action]

initial = (0,0)

big = fst
small = snd

totalWater (big,small) = big + small
action :: Action -> State -> State
action Initialize _ = initial
action FillSmallJug (big,_) = (big,3)
action FillBigJug (_,small) = (5,small)
action PourSmallIntoBig (big, small) = let qty = min (5-big) small in (big+qty, small-qty)
action PourBigIntoSmall (big, small) = let qty = min (3-small) big in (big-qty, small+qty)
action EmptySmallJug (big, _) = (big, 0)
action EmptyBigJug (_,small) = (0, small)

nextActions :: [State] -> [Action] 
nextActions states = [a | a <- map toEnum [0..6], not ((action a (last states)) `elem` states)]

states :: [Action] -> [State]
states as = scanl (\st a -> action a st) initial as 

nextPossibleActions :: [Action] -> [Action]
nextPossibleActions as = nextActions $ states as

explore :: [Path]
explore  = explore' [[]]

explore' :: [Path] -> [Path]
explore' [] = []
explore' (p:ps) = case nextPossibleActions p of
    [] -> p : explore' ps
    as -> (explore' (map (\a -> p ++ [a]) as)) ++ explore' ps


