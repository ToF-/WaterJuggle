import Test.QuickCheck
import WaterJuggle

prop_initial = big initial == 0 && small initial == 0

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
    && if small state' > small state then big state' > big state else True

main = do
    quickCheck prop_initial
    quickCheck prop_fillBig
    quickCheck prop_fillSmall
    quickCheck prop_emptyBig
    quickCheck prop_emptySmall
    quickCheck prop_pourSmallInBig
    quickCheck prop_pourBigInSmall
    
