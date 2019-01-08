import Test.QuickCheck

data State = State { big :: Integer, small :: Integer }
    deriving (Show)

data Action = FillBig | FillSmall | EmptyBig

instance Arbitrary State where
    arbitrary = do 
        b <- choose (0, 5)
        s <- choose (0, 3)
        return $ State b s

action :: Action -> State -> State
action FillBig   (State _ s) = State 5 s
action FillSmall (State b _) = State b 3
action EmptyBig  (State _ s) = State 0 s

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

    
