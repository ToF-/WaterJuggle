import Test.QuickCheck
import WaterJuggle
import Data.List

doCheckStateProperty :: String -> (State -> Bool) -> IO ()
doCheckStateProperty s p = do
    putStr (s ++ "\t : ")
    quickCheck (p . forSomeCorrectState)

doCheckActionsProperty :: String -> ([Action] -> Bool) -> IO ()
doCheckActionsProperty s p = do
    putStr (s ++ "\t : ")
    quickCheck p
    
invariant :: State -> Bool
invariant (big,small) = big >= 0 && big <= 5 && small >= 0 && small <= 3

forSomeCorrectState :: State -> State
forSomeCorrectState (big,small) = ((abs big) `mod` 6, (abs small) `mod` 4)

instance Arbitrary Action
    where
    arbitrary = do
        n <- choose (0,6)
        return $ toEnum n

arbitraryList :: Gen [Action]
arbitraryList =
    do
      a <- arbitrary
      return $ head $ explore' [[a]]

main = do 
    mapM_ (uncurry doCheckStateProperty) [
     ("big jug is initially empty", \_ -> big initial == 0)
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
     ,("initialize empty jugs",
         \state -> let state' = action Initialize state
         in state' == initial) 
     ,("next actions given an initial step gives no repeated state",
         \state -> let states = map (\a -> action a initial) (nextActions [initial])    
         in not (initial `elem` states) && not (null states))
     ,("next actions given any step gives no repeated state",
         \state -> let states = map (\a -> action a state) (nextActions [state])    
         in not (state `elem` states) && not (null states))
     ]
    mapM_ (uncurry doCheckActionsProperty) [
        ("states given an empty list of actions gives initial state",
         \_ -> states [] == [initial])
        ,("states gives a list of same length than action ++ initial",
         \actions -> length (states actions) == 1 + length actions )
        ,("next possible actions always lead to unprecedented state",
         \actions -> let actions' = nextPossibleActions actions
                         states'   = states actions
                         lastAction = last states'
         in if not (null actions) then all  (\a -> not ((action a lastAction) `elem` states')) actions' else True) 
       ,("explore does not contain any state where big jug = 4",
        \actions -> let paths = explore' [actions]
          in if not (null actions) then all (\path -> all (\st -> (big st == 0)) (states path)) paths else True)
     ]
