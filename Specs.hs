import Test.QuickCheck

data State = State { big :: Integer, small :: Integer }
    deriving (Show,Eq)

data Action = FillBig | FillSmall | EmptyBig | EmptySmall | PourBigSmall | PourSmallBig

instance Arbitrary State where
    arbitrary = do 
        b <- choose (0, 5)
        s <- choose (0, 3)
        return $ State b s

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

main = do
    putStr "dummy check\t"
    quickCheck True
    putStr "state invariant\t"
    quickCheck prop_StateInvariant
    putStr "filling big makes big = 5\t"
    quickCheck prop_FillBig
    putStr "filling big makes small = 3\t"
    quickCheck prop_FillSmall
    putStr "empty big makes big = 0\t"
    quickCheck prop_EmptyBig
    putStr "empty small makes small = 0\t"
    quickCheck prop_EmptySmall
    putStr "pour big into small doesn't change state when big = 0 or small = 3\n"
    putStr "pour big into small makes small bigger and big smaller\t" 
    quickCheck prop_PourBigSmall
    putStr "pour small into big doesn't change state when big = 5 or small = 0\n"
    putStr "pour small into big makes big bigger and small smaller\t" 
    quickCheck prop_PourSmallBig

    
