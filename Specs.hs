import Test.QuickCheck

data State = State { big :: Integer, small :: Integer }
    deriving (Show)

data Action = FillBig

instance Arbitrary State where
    arbitrary = do 
        b <- choose (0, 5)
        s <- choose (0, 3)
        return $ State b s

action :: Action -> State -> State
action FillBig (State _ s) = State 5 s

prop_StateInvariant :: State -> Bool
prop_StateInvariant st = big st >= 0 && big st <= 5 && small st >= 0 && small st <= 3

prop_FillBig :: State -> Bool
prop_FillBig st = let st' = action FillBig st 
                    in big st' == 5 && small st' == small st

main = do
    putStr "dummy check\t"
    quickCheck True
    putStr "state invariant\t"
    quickCheck prop_StateInvariant
    putStr "filling big makes big = 5"
    quickCheck prop_FillBig

    
