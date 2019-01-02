import Test.QuickCheck
import WaterJuggle

doCheck :: String -> (State -> Bool) -> IO ()
doCheck s p = do
    putStr (s ++ "\t : ")
    quickCheck p

main = mapM_ (uncurry doCheck)
    [("big jug is initially empty", \_ -> big initial == 0)
    ,("small jug is initially empty", \_ -> small initial == 0)
    ,("fill small jug makes it 3 and leaves big jug unchanged", 
        \state -> let state' = action FillSmallJug state
        in small state' == 3 && big state' == big state)
    ,("fill big jug makes it 5 and leaves small jug unchanged", 
        \state -> let state' = action FillBigJug state
        in big state' == 5 && small state' == small state)
    ,("pour small into big, makes big no bigger than five",
        \state -> let state' = action PourSmallIntoBig state
        in big state' <= 5)
    ,("pour non zero small into big, makes big bigger and small smaller ",
        \state -> let state' = action PourSmallIntoBig state
        in if small state > 0 && big state < 5 then big state' > big state && small state' < small state else True)
    ]
