import Test.QuickCheck
import WaterJuggle

doCheck :: String -> (State -> Bool) -> IO ()
doCheck s p = do
    putStr (s ++ "\t : ")
    quickCheck (p . forSomeCorrectState)

invariant :: State -> Bool
invariant (big,small) = big >= 0 && big <= 5 && small >= 0 && small <= 3

forSomeCorrectState :: State -> State
forSomeCorrectState (big,small) = ((abs big) `mod` 6, (abs small) `mod` 4)

main = mapM_ (uncurry doCheck)
    [("big jug is initially empty", \_ -> big initial == 0)
    ,("small jug is initially empty", \_ -> small initial == 0)
    ,("fill small jug makes it 3 and leaves big jug unchanged", 
        \state -> let state' = action FillSmallJug state
        in small state' == 3 && big state' == big state)
    ,("fill big jug makes it 5 and leaves small jug unchanged", 
        \state -> let state' = action FillBigJug state
        in big state' == 5 && small state' == small state)
    ,("pour small into big preserves invariant",
        \state -> let state' = action PourSmallIntoBig state
        in invariant state' && totalWater state' == totalWater state)
    ,("pour non zero small into big, makes big bigger and small smaller",
        \state -> let state' = action PourSmallIntoBig state
        in if small state > 0 && big state < 5 then big state' > big state && small state' < small state else state' == state && totalWater state' == totalWater state)
    ,("pour big into small preserves invariant",
        \state -> let state' = action PourBigIntoSmall state
        in invariant state' && totalWater state' == totalWater state)
    ,("pour non zero big into small, makes small bigger, and big smaller",
        \state -> let state' = action PourBigIntoSmall state
         in if big state > 0 && small state < 3 then small state' > small state && big state' < big state else state' == state && totalWater state' == totalWater state)
    ,("empty small jug makes it zero",
        \state -> let state' = action EmptySmallJug state 
        in small state' == 0 && big state' == big state)
    ,("empty big jug makes it zero",
        \state -> let state' = action EmptyBigJug state 
        in big state' == 0 && small state' == small state)
    ]
