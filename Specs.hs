import Test.QuickCheck
import Data.List (sort, nub)

data State = State { big :: Integer, small :: Integer }
    deriving (Show,Eq,Ord)

data Action = FillBig | FillSmall | EmptyBig | EmptySmall | PourBigSmall | PourSmallBig
    deriving (Eq,Show, Enum, Bounded)

data Path = Path { actions :: [Action] }
    deriving (Eq, Show)

instance Arbitrary State where
    arbitrary = choose (0, 5) >>= \b -> choose (0,3) >>= \s -> return (State b s) 

instance Arbitrary Action where
    arbitrary = elements [minBound .. maxBound]

computeStates :: [Action] -> [State]
computeStates as = scanl (\st a -> action a st) (State 0 0) as

possibleActions :: [State] -> [Action]
possibleActions sts = filter (\a -> let st = action a (last sts) in not (st `elem` sts)) (map toEnum [0..5])

nextAction :: [Action] -> Gen [Action]
nextAction as = do
    let states = computeStates as 
        possibles = possibleActions states
    if null possibles || any (\st -> big st == 4) states then return as else do 
                                    a <- elements possibles
                                    as' <- nextAction (as ++ [a])
                                    return as'

instance Arbitrary Path where
    arbitrary = nextAction [] >>= return . Path
            

action :: Action -> State -> State
action FillBig    (State _ s) = State 5 s
action FillSmall  (State b _) = State b 3
action EmptyBig   (State _ s) = State 0 s
action EmptySmall (State b _) = State b 0
action PourBigSmall (State b s) = let q = min b (3-s) in State (b-q) (s+q)
action PourSmallBig (State b s) = let q = min s (5-b) in State (b+q) (s-q)

prop_StateInvariant :: State -> Bool
prop_StateInvariant st = big st >= 0 && big st <= 5 && small st >= 0 && small st <= 3

prop_FillBig :: State -> Bool
prop_FillBig st = let st' = action FillBig st 
                    in big st' == 5 && small st' == small st

prop_FillSmall :: State -> Bool
prop_FillSmall st = let st' = action FillSmall st 
                     in big st' == big st && small st' == 3

prop_EmptyBig :: State -> Bool
prop_EmptyBig st = let st' = action EmptyBig st 
                    in big st' == 0 && small st' == small st

prop_EmptySmall :: State -> Bool
prop_EmptySmall st = let st' = action EmptySmall st 
                    in big st' == big st && small st' == 0

prop_PourBigSmall :: State -> Bool
prop_PourBigSmall st@(State 0 _) = action PourBigSmall st == st
prop_PourBigSmall st@(State _ 3) = action PourBigSmall st == st
prop_PourBigSmall st = let st' = action PourBigSmall st
    in small st' > small st && big st' < big st

prop_PourSmallBig :: State -> Bool
prop_PourSmallBig st@(State 5 _) = action PourSmallBig st == st
prop_PourSmallBig st@(State _ 0) = action PourSmallBig st == st
prop_PourSmallBig st = let st' = action PourSmallBig st
    in small st' < small st && big st' > big st

prop_Path :: Path -> Bool
prop_Path path = let states = computeStates (actions path) in (nub . sort) states == sort states

prop_Solved :: Path -> Bool
prop_Solved path = not (any (\st -> big st == 4) (computeStates (actions path)))
main = do
    putStr "dummy check\n"
    quickCheck True
    putStr "state invariant\n"
    quickCheck prop_StateInvariant
    putStr "filling big makes big = 5\n"
    quickCheck prop_FillBig
    putStr "filling big makes small = 3\n"
    quickCheck prop_FillSmall
    putStr "empty big makes big = 0\n"
    quickCheck prop_EmptyBig
    putStr "empty small makes small = 0\n"
    quickCheck prop_EmptySmall
    putStr "pour big into small doesn't change state when big = 0 or small = 3\n"
    putStr "pour big into small makes small bigger and big smaller\n" 
    quickCheck prop_PourBigSmall
    putStr "pour small into big doesn't change state when big = 5 or small = 0\n"
    putStr "pour small into big makes big bigger and small smaller\n" 
    quickCheck prop_PourSmallBig
    putStr "a path contains 0 to N actions\n"
    putStr "a path contains no action that lead to a repeated state\n"
    quickCheck prop_Path
    putStr "a path contains no action that lead to a state where big == 4\n"
    quickCheck prop_Solved
    

    
