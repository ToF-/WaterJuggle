import Test.QuickCheck
import WaterJuggle

prop_initial _ = big initial == 0 && small initial == 0

prop_fillBig :: State -> Bool
prop_fillBig state = big (fillBig state) == 5 && small (fillBig state) == small state

prop_fillSmall :: State -> Bool
prop_fillSmall state = small (fillSmall state) == 3 && big (fillSmall state) == big state

prop_emptyBig :: State -> Bool
prop_emptyBig state = big (emptyBig state) == 0 && small (emptyBig state) == small state

prop_emptySmall :: State -> Bool
prop_emptySmall state = small (emptySmall state) == 0 && big (emptySmall state) == big state

prop_pourSmallInBig :: State -> Bool
prop_pourSmallInBig s = let state = (abs (big s) `mod` 6, abs (small s) `mod` 4)
                            state' = pourSmallInBig state
    in big state <= 5 && small state <= 3
    && if small state > 0 && big state < 5 then big state' > big state else True
    && if big state' > big state then small state' < small state else True
    && (big state' - big state) == (small state - small state')

prop_pourBigInSmall :: State -> Bool
prop_pourBigInSmall s = let state = (abs (big s) `mod` 6, abs (small s) `mod` 4)
                            state' = pourBigInSmall state
    in big state <= 5 && small state <= 3
    && if big state > 0 && small state < 3 then small state' > small state else True
    && if small state' > small state then big state' < big state else True

prop_fillActions :: State -> Bool
prop_fillActions _ = let state = actions [FillSmall,FillBig] initial
    in small state == 3 && big state == 5

prop_pourBigInSmallActions :: State -> Bool
prop_pourBigInSmallActions _ = let state = actions [FillBig,PourBigInSmall] initial
    in big state == 2 && small state == 3

main = mapM_ quickCheck 
    [prop_initial
    ,prop_fillBig
    ,prop_fillSmall
    ,prop_emptyBig
    ,prop_emptySmall
    ,prop_pourSmallInBig
    ,prop_pourBigInSmall
    ,prop_fillActions
    ,prop_pourBigInSmallActions
    ]
